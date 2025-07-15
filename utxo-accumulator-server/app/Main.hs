import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Options
import ZkFold.Cardano.UtxoAccumulator.Server.Run (runServer)

main :: IO ()
main = do
  serverOptions <- execParser opts
  runServer serverOptions
 where
  opts =
    info
      (parseServerOptions <**> helper)
      ( fullDesc
          <> progDesc "UTxO Accumulator server with concurrent accumulation and distribution"
          <> header "UTxO Accumulator"
      )
