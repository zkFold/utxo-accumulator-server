module Main (main) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (defaultMain, testGroup)
import ZkFold.Cardano.UtxoAccumulator.Test (utxoAccumulatorTests)

main :: IO ()
main = do
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $
      testGroup
        "utxo-accumulator-tests"
        [ utxoAccumulatorTests setup
        ]
