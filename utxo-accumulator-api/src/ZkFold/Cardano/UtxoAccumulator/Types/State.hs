module ZkFold.Cardano.UtxoAccumulator.Types.State where

import PlutusLedgerApi.V1 (TxOutRef)

type UtxoAccumulatorState = Maybe TxOutRef
