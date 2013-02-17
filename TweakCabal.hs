{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module TweakCabal where

import           ClassyPrelude
import           Debug.Trace
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.PrettyPrint
import           Distribution.Text                           (display)
import           Distribution.Version

data TweakCabalSettings = TweakCabalSettings
    { tcsNoBounds :: Set Text -- ^ package names
    }

tweakCabal :: TweakCabalSettings -> LByteString -> LByteString
tweakCabal TweakCabalSettings {..} bs = fromMaybe bs $ do
    gpd <-
        case parsePackageDescription $ unpack $ decodeUtf8 bs of
            ParseFailed _ -> Nothing
            ParseOk _ x -> Just x
    let string = showGenericPackageDescription gpd
            { condLibrary = tweakCondTree <$> condLibrary gpd
            , condExecutables = second tweakCondTree <$> condExecutables gpd
            , condTestSuites = second tweakCondTree <$> condTestSuites gpd
            , condBenchmarks = second tweakCondTree <$> condBenchmarks gpd
            }
    -- Following added for:
    -- https://github.com/haskell/cabal/issues/1202
    case parsePackageDescription string of
            ParseFailed _ -> trace
                ("Cabal bug: could not parse then pretty-print: " ++ display (package $ packageDescription gpd))
                Nothing
            ParseOk _ _ -> Just $ encodeUtf8 $ pack string
  where
    tweakCondTree ct = ct
        { condTreeConstraints = tweakConstraints $ condTreeConstraints ct
        , condTreeComponents = tweakComponents <$> condTreeComponents ct
        }

    tweakConstraints = map tweakDependency

    tweakDependency orig@(Dependency name'@(PackageName name) _)
        | pack name `elem` tcsNoBounds = Dependency name' anyVersion
        | otherwise = orig

    tweakComponents (a, b, c) =
        ( a
        , tweakCondTree b
        , tweakCondTree <$> c
        )
