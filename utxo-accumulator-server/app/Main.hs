import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Options

main :: IO ()
main = runCommand =<< execParser opts
 where
  opts =
    info
      (parseCommand <**> helper)
      ( fullDesc
          <> progDesc "UTxO Accumulator helpful operations"
          <> header "UTxO Accumulator"
      )
