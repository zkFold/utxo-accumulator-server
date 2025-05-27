module ZkFold.Cardano.UtxoAccumulator.Types.State where

import GeniusYield.Types (GYTxOutRef)

type UtxoAccumulatorState = Maybe GYTxOutRef
