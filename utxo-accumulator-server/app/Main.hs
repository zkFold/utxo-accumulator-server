import Options.Applicative
import ZkFold.Cardano.UtxoAccumulator.Server.Options
import ZkFold.Cardano.UtxoAccumulator.Server.Run (initAccumulator, postScript, runServer)

main :: IO ()
main = do
  options <- execParser opts
  case optCommand options of
    RunServer serverOpts -> runServer serverOpts
    PostScript postScriptOpts -> postScript postScriptOpts
    InitAccumulator initOpts -> initAccumulator initOpts
 where
  opts =
    info
      (parseOptions <**> helper)
      ( fullDesc
          <> progDesc "UTxO Accumulator server with concurrent accumulation and distribution"
          <> header "UTxO Accumulator"
      )
