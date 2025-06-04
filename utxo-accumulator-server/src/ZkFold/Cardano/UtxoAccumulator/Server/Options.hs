module ZkFold.Cardano.UtxoAccumulator.Server.Options (
  parseCommand,
  runCommand,
) where

import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Run (Mode (..), runServer)

data Command = Accumulate (Maybe FilePath) | Distribute (Maybe FilePath) Bool

parseCommandOptions :: Parser (Maybe FilePath)
parseCommandOptions =
  optional
    ( strOption
        ( long "config"
            <> metavar "CONFIG"
            <> short 'c'
            <> help "Path of optional configuration file. If not provided, \"SERVER_CONFIG\" environment variable is used."
        )
    )

parseDistributeOptions :: Parser (Maybe FilePath, Bool)
parseDistributeOptions =
  (,)
    <$> parseCommandOptions
    <*> switch
      ( long "remove-no-date"
      <> help "If set, distribute (remove) UTxOs with no removal date. Default: do not remove UTxOs with no date."
      )

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ command
          "accumulate"
          ( info (Accumulate <$> parseCommandOptions <**> helper) $
              progDesc "Serve endpoints"
          )
      , command
          "distribute"
          ( info (uncurry Distribute <$> parseDistributeOptions <**> helper) $
              progDesc "Distribute endpoints"
          )
      ]

runCommand :: Command -> IO ()
runCommand (Accumulate mcfp) = runServer mcfp ModeAccumulate
runCommand (Distribute mcfp removeNoDate) = runServer mcfp (ModeDistribute removeNoDate)
