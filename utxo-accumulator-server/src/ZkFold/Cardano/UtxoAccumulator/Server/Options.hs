module ZkFold.Cardano.UtxoAccumulator.Server.Options (
  parseOptions,
  Options (..),
  Command (..),
  ServerOptions (..),
  PostScriptOptions (..),
  InitAccumulatorOptions (..),
) where

import Options.Applicative

data Options = Options
  { optCommand :: Command
  }

data Command
  = RunServer ServerOptions
  | PostScript PostScriptOptions
  | InitAccumulator InitAccumulatorOptions

data ServerOptions = ServerOptions
  { soConfigPath :: FilePath
  , soForceDistribute :: Bool
  , soCleanDb :: Bool
  }

data PostScriptOptions = PostScriptOptions
  { psoConfigPath :: FilePath
  }

data InitAccumulatorOptions = InitAccumulatorOptions
  { iaoConfigPath :: FilePath
  , iaoNumTokens :: Int
  }

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "run" (info parseRunServer (progDesc "Run the UTxO Accumulator server"))
        <> command "post-script" (info parsePostScript (progDesc "Post the UTxO Accumulator script"))
        <> command "init" (info parseInitAccumulator (progDesc "Initialize the UTxO Accumulator"))
    )

parseRunServer :: Parser Command
parseRunServer = RunServer <$> parseServerOptions

parsePostScript :: Parser Command
parsePostScript = PostScript <$> parsePostScriptOptions

parseInitAccumulator :: Parser Command
parseInitAccumulator = InitAccumulator <$> parseInitAccumulatorOptions

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

parsePostScriptOptions :: Parser PostScriptOptions
parsePostScriptOptions =
  PostScriptOptions
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> short 'c'
          <> help "Path of configuration file."
      )

parseInitAccumulatorOptions :: Parser InitAccumulatorOptions
parseInitAccumulatorOptions =
  InitAccumulatorOptions
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> short 'c'
          <> help "Path of configuration file."
      )
    <*> option
      auto
      ( long "num-tokens"
          <> metavar "NUM"
          <> short 'n'
          <> value 1
          <> help "Number of thread tokens to create (default: 1)."
      )
