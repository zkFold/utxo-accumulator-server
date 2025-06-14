module Main where

import Data.Foldable (toList)
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N)
import ZkFold.Prelude (readFileJSON, writeFileJSON)
import ZkFold.Symbolic.Examples.UtxoAccumulator

main :: IO ()
main = do
  gsData <- readFileJSON "utxo-accumulator-api/data/bls12381-g1_n65542.json"
  h1Data <- readFileJSON "utxo-accumulator-api/data/bls12381-g2_n2.json"
  let crs = UtxoAccumulatorCRS gsData h1Data [] []
      crs' =
        crs
          { crsAccElems = toList $ accumulationGroupElements @N @M crs
          , crsDistElems = toList $ distributionGroupElements @N @M crs
          }
  writeFileJSON "utxo-accumulator-api/data/crs.json" crs'
