module ZkFold.Cardano.UtxoAccumulator.Types.Monad where

import Control.Monad.Reader (MonadReader)
import GeniusYield.TxBuilder (GYTxQueryMonad)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Ctx)

type UtxoAccumulatorQueryMonad m = (GYTxQueryMonad m, MonadReader Ctx m)
