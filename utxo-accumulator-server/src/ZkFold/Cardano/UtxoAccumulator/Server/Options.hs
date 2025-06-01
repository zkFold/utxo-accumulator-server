module ZkFold.Cardano.UtxoAccumulator.Server.Options (
  parseCommand,
  runCommand,
) where

import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Run (Mode (..), runServer)

data Command = Accumulate (Maybe FilePath) | Distribute (Maybe FilePath)

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
          ( info (Distribute <$> parseCommandOptions <**> helper) $
              progDesc "Distribute endpoints"
          )
      ]

runCommand :: Command -> IO ()
runCommand (Accumulate mcfp) = runServer mcfp ModeAccumulate
runCommand (Distribute mcfp) = runServer mcfp ModeDistribute
