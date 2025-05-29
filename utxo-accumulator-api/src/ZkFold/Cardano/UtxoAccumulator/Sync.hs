module ZkFold.Cardano.UtxoAccumulator.Sync where

import Control.Monad.Reader (ask)
import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxSkeleton, addressFromPlutus', mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Api.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Constants
import ZkFold.Cardano.UtxoAccumulator.Transition (mkAddUtxo, mkRemoveUtxo, mkSwitchAccumulator)
import ZkFold.Cardano.UtxoAccumulator.Types (UtxoAccumulatorQueryMonad)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

data AccumulatorInput = Add (ScalarFieldOf BLS12_381_G1_Point)
                      | Remove (ScalarFieldOf BLS12_381_G1_Point)
