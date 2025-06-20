module Main where

import Data.Foldable (toList)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N)
import ZkFold.Prelude (readFileJSON, writeFileJSON)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (acSizeN)
import ZkFold.Symbolic.Examples.UtxoAccumulator

main :: IO ()
main = do
  print $ acSizeN $ utxoAccumulatorCircuit @N @(ScalarFieldOf BLS12_381_G1_Point)
  !gsData <- readFileJSON "utxo-accumulator-api/data/bls12381-g1_n65542.json"
  !h1Data <- readFileJSON "utxo-accumulator-api/data/bls12381-g2_n2.json"
  let crs = UtxoAccumulatorCRS gsData h1Data [] []
      crs' =
        crs
          { crsAccElems = toList $ accumulationGroupElements @N @M crs
          , crsDistElems = toList $ distributionGroupElements @N @M crs
          }
  writeFileJSON "utxo-accumulator-api/data/crs.json" crs'
