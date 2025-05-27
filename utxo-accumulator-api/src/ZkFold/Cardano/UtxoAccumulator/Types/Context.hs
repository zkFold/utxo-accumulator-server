module ZkFold.Cardano.UtxoAccumulator.Types.Context where

import GeniusYield.Types (GYTxOutRef)
import PlutusLedgerApi.V3 (Value)

data Context = Context
  { ctxThreadTokenRef :: GYTxOutRef
  , ctxAccumulationValue :: Value
  }
  deriving (Show, Eq)
