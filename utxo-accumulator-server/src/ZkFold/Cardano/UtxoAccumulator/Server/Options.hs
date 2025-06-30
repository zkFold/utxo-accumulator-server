module ZkFold.Cardano.UtxoAccumulator.Server.Options (
  parseCommand,
  runCommand,
) where

import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Run (Mode (..), runServer)

-- Add a Bool for cleanup to Distribute
-- (Maybe FilePath, Bool, Bool): config, distribute-no-date, cleanup

data Command = Accumulate (Maybe FilePath) | Distribute (Maybe FilePath) Bool Bool

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

parseDistributeOptions :: Parser Command
parseDistributeOptions =
  Distribute
    <$> parseCommandOptions
    <*> switch
      ( long "force"
          <> help "If set, distribute (remove) all valid UTxOs. Default: don't distribute UTxOs with the timer in the future or with no timer."
      )
    <*> switch
      ( long "clean-db"
          <> help "If set, clean the transaction database from old transactions and those with no timer before distributing. Default: do not clean the database."
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
          ( info (parseDistributeOptions <**> helper) $
              progDesc "Distribute endpoints"
          )
      ]

runCommand :: Command -> IO ()
runCommand (Accumulate mcfp) = runServer mcfp ModeAccumulate
runCommand (Distribute mcfp forceDist cleanDb) = runServer mcfp (ModeDistribute forceDist cleanDb)
