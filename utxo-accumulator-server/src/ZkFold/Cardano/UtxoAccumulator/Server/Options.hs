module ZkFold.Cardano.UtxoAccumulator.Server.Options (
  parseServerOptions,
  ServerOptions (..),
) where

import Options.Applicative

data ServerOptions = ServerOptions
  { soConfigPath :: FilePath
  , soForceDistribute :: Bool
  , soCleanDb :: Bool
  }

parseServerOptions :: Parser ServerOptions
parseServerOptions =
  ServerOptions
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> short 'c'
          <> help "Path of configuration file."
      )
    <*> switch
      ( long "force"
          <> help "If set, distributes all valid UTxOs regardless of their timer status. Default: not set."
      )
    <*> switch
      ( long "clean-db"
          <> help "If set, cleans the transaction database from old transactions and those with no timer before distributing. Default: not set."
      )
