{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module HackageProxy where

import           Blaze.ByteString.Builder (fromByteString, fromLazyByteString)
import           BasicPrelude
import qualified Codec.Archive.Tar        as Tar
import           Codec.Compression.GZip   (compress)
import qualified Data.ByteString          as S
import           Data.CaseInsensitive     (CI)
import           Data.Conduit
import           Data.Conduit.Lazy        (lazyConsume)
import           Data.Conduit.Zlib        (ungzip)
import           Data.Text                (breakOnEnd)
import           Network.HTTP.Conduit
import           Network.HTTP.Client      (responseOpen, responseClose)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Types       (status200)
import           Network.Wai              (responseSourceBracket, pathInfo,
                                           rawPathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)
import           System.FilePath          (takeExtension)
import           TweakCabal
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as L

data HackageProxySettings = HackageProxySettings
    { hpsPort     :: Int
    , hpsNoBounds :: Set Text
    , hpsSource   :: Text
    }

runHackageProxy :: HackageProxySettings -> IO ()
runHackageProxy HackageProxySettings {..} = do
    baseReq <- parseUrl $ T.unpack hpsSource
    run hpsPort $ app baseReq
        { checkStatus = \_ _ _ -> Nothing
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
    app baseReq waiReq =
      bracket (newManager conduitManagerSettings) closeManager $ \man -> do
      responseSourceBracket (responseOpen req man) responseClose $ \res -> do
        let src = bodyReaderSource $ responseBody res

            resStatus = responseStatus res
            resHeaders = (filter ((`HashSet.member` safeResponseHeaders) . fst) (responseHeaders res))

        src' <- if isTarball res
            then do
                lbs <- L.fromChunks <$> lazyConsume (src $= ungzip)
                entries <- mapEntries tweakEntry $ Tar.read lbs
                return $ yield $ Chunk $ fromLazyByteString $ compress $ Tar.write entries
            else return $ mapOutput (Chunk . fromByteString) src

        return (resStatus, resHeaders, src')
      where
        isTarball res =
            ".tar.gz" `S.isSuffixOf` rawPathInfo waiReq
            && responseStatus res == status200
        req = baseReq { path = path baseReq `combine` rawPathInfo' }
        rawPathInfo' =
            case pathInfo waiReq of
                ["package", T.stripSuffix ".tar.gz" -> Just packver] | Just package <- mpackage ->
                    encodeUtf8 $ intercalate "/" [package, version, packver ++ ".tar.gz"]
                  where
                    (T.stripSuffix "-" -> mpackage, version) = breakOnEnd "-" packver
                _ -> rawPathInfo waiReq
        combine a b
            | "/" `S.isSuffixOf` a || "/" `S.isPrefixOf` b = a ++ b
            | otherwise = mconcat [a, "/", b]

    tweakEntry e@(Tar.entryContent -> Tar.NormalFile lbs _)
        | takeExtension (Tar.entryPath e) == ".cabal" = e
            { Tar.entryContent = Tar.NormalFile lbs' $ L.length lbs'
            }
      where
        lbs' = tweakCabal tcs lbs
    tweakEntry e = e

    mapEntries f (Tar.Next entry rest) = (f entry:) <$> mapEntries f rest
    mapEntries _ Tar.Done = return []
    mapEntries _ (Tar.Fail e) = throwIO e

safeResponseHeaders :: HashSet (CI ByteString)
safeResponseHeaders = HashSet.fromList
    [ "content-type"
    , "etag"
    , "expires"
    , "last-modified"
    ]
