module Main where

import ZkFold.Cardano.UtxoAccumulator.Constants (N, M, utxoAccumulatorCRS)
import ZkFold.Prelude (writeFileJSON)
import ZkFold.Symbolic.Examples.UtxoAccumulator (accumulationGroupElements, distributionGroupElements)

main :: IO ()
main = do
    crs <- utxoAccumulatorCRS

    writeFileJSON "utxo-accumulator-api/data/accumulation-group-elements.json" (accumulationGroupElements @N @M crs)
    writeFileJSON "utxo-accumulator-api/data/distribution-group-elements.json" (distributionGroupElements @N @M crs)
