{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           ClassyPrelude
import           HackageProxy
import           Options.Applicative

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
    <*> (pack <$> strOption
        ( long "source"
       <> help "Hackage source URL we're proxying from"
       <> short 's'
       <> value "http://hackage.haskell.org/packages/archive"
       <> metavar "URL"
        ))
  where
    fixNoBounds [] = pack $ words "base process Cabal"
    fixNoBounds x = pack $ map pack x

main :: IO ()
main = do
    hps <- execParser opts
    putStrLn $ "Listening on port: " ++ show (hpsPort hps)
    putStrLn $ "Dropping bounds for: " ++ unwords (unpack $ hpsNoBounds hps)
    putStrLn $ "Downloading from: " ++ hpsSource hps
    runHackageProxy hps
  where
    opts = info (helper <*> hackageProxySettings)
        ( fullDesc
       <> progDesc "Run a Hackage proxy, modifying packages in some way."
       <> header "hackage-proxy - Proxy for Hackage packages."
        )
