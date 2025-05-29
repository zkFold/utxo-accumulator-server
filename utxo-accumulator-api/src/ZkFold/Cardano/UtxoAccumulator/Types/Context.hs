module ZkFold.Cardano.UtxoAccumulator.Types.Context where

import GeniusYield.Types (GYTxOutRef, GYValue)

data Context = Context
  { ctxAccumulatorScriptRef :: GYTxOutRef
  , ctxThreadTokenRef       :: GYTxOutRef
  , ctxAccumulationValue    :: GYValue
  }
  deriving (Show, Eq)
