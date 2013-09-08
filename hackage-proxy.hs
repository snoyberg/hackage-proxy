{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude
import           HackageProxy
import           Options.Applicative
import qualified Data.Text as T
import qualified Data.Set as Set

hackageProxySettings :: Parser HackageProxySettings
hackageProxySettings = HackageProxySettings
    <$> option
        ( long "port"
       <> help "Port to listen on"
       <> short 'p'
       <> value 4200
       <> metavar "PORT"
        )
    <*> (fixNoBounds <$> many (strOption
        ( long "no-bounds"
       <> help "Packages to drop version bounds from"
       <> short 'n'
       <> metavar "PACKAGE"
        )))
    <*> (T.pack <$> strOption
        ( long "source"
       <> help "Hackage source URL we're proxying from"
       <> short 's'
       <> value "http://hackage.haskell.org/packages/archive"
       <> metavar "URL"
        ))
  where
    fixNoBounds [] = Set.fromList $ words "base process Cabal directory template-haskell"
    fixNoBounds x = Set.fromList $ map T.pack x

main :: IO ()
main = do
    hps <- execParser opts
    putStrLn $ "Listening on port: " ++ show (hpsPort hps)
    putStrLn $ "Dropping bounds for: " ++ unwords (Set.toList $ hpsNoBounds hps)
    putStrLn $ "Downloading from: " ++ hpsSource hps
    runHackageProxy hps
  where
    opts = info (helper <*> hackageProxySettings)
        ( fullDesc
       <> progDesc "Run a Hackage proxy, modifying packages in some way."
       <> header "hackage-proxy - Proxy for Hackage packages."
        )
