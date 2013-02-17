{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module HackageProxy where

import           Blaze.ByteString.Builder (fromByteString)
import           ClassyPrelude
import qualified Codec.Archive.Tar        as Tar
import           Codec.Compression.GZip   (compress)
import qualified Data.ByteString          as S
import           Data.CaseInsensitive     (CI)
import           Data.Conduit
import           Data.Conduit.Lazy        (lazyConsume)
import           Data.Conduit.Zlib        (ungzip)
import           Data.Text                (breakOnEnd)
import           Network.HTTP.Conduit
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Response (ResponseSource), pathInfo,
                                           rawPathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)
import           System.FilePath          (takeExtension)
import           TweakCabal

data HackageProxySettings = HackageProxySettings
    { hpsPort     :: Int
    , hpsNoBounds :: Set Text
    , hpsSource   :: Text
    }

runHackageProxy :: HackageProxySettings -> IO ()
runHackageProxy HackageProxySettings {..} = do
    baseReq <- parseUrl $ unpack hpsSource
    run hpsPort $ app baseReq
        { checkStatus = \_ _ -> Nothing
        -- Sometimes Hackage can be slow at responding.
        , responseTimeout = Just 30000000
        }
  where
    tcs = TweakCabalSettings hpsNoBounds

    -- Keep-alive connections with the main Hackage server do not seem to work,
    -- so for now we're just creating a new manager for each connection. Note
    -- that we're /not/ using withManager: we want to reuse the original
    -- ResourceT of the WAI app so that we can keep resources open from the
    -- request to response.
    app baseReq waiReq = bracket (lift $ newManager def) (lift . closeManager) $ \man -> do
        res <- http req man
        (src, _) <- unwrapResumable $ responseBody res

        let resStatus = responseStatus res
            resHeaders = (filter ((`elem` safeResponseHeaders) . fst) (responseHeaders res))

        if isTarball res
            then do
                lbs <- fromChunks <$> lazyConsume (src $= ungzip)
                entries <- mapEntries tweakEntry $ Tar.read lbs
                return $ responseLBS resStatus resHeaders (compress $ Tar.write entries)
            else return $ ResponseSource resStatus resHeaders (mapOutput (Chunk . fromByteString) src)
      where
        isTarball res =
            ".tar.gz" `isSuffixOf` rawPathInfo waiReq
            && responseStatus res == status200
        req = baseReq { path = path baseReq `combine` rawPathInfo' }
        rawPathInfo' =
            case pathInfo waiReq of
                ["package", stripSuffix ".tar.gz" -> Just packver] | Just package <- mpackage ->
                    encodeUtf8 $ intercalate "/" [package, version, packver ++ ".tar.gz"]
                  where
                    (stripSuffix "-" -> mpackage, version) = breakOnEnd "-" packver
                _ -> rawPathInfo waiReq
        combine a b
            | "/" `isSuffixOf` a || "/" `S.isPrefixOf` b = a ++ b
            | otherwise = concat [a, "/", b]

    tweakEntry e@(Tar.entryContent -> Tar.NormalFile lbs _)
        | takeExtension (Tar.entryPath e) == ".cabal" = e
            { Tar.entryContent = Tar.NormalFile lbs' $ length lbs'
            }
      where
        lbs' = tweakCabal tcs lbs
    tweakEntry e = e

    mapEntries f (Tar.Next entry rest) = (f entry:) <$> mapEntries f rest
    mapEntries _ Tar.Done = return []
    mapEntries _ (Tar.Fail e) = throwIO e

safeResponseHeaders :: HashSet (CI ByteString)
safeResponseHeaders = pack
    [ "content-type"
    , "etag"
    , "expires"
    , "last-modified"
    ]
