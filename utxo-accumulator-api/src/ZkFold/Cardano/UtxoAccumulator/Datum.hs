module ZkFold.Cardano.UtxoAccumulator.Datum where

import Data.Foldable (foldr')
import PlutusLedgerApi.V3 (ToData (..))
import PlutusTx.Prelude (
  BuiltinByteString,
  Maybe (..),
  blake2b_224,
  head,
  ($),
  (.),
 )
import Prelude (map, (==))

import Data.List (find)
import Data.Maybe (fromJust)
import PlutusTx.Builtins (serialiseData)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Cardano.OffChain.BLS12_381 (convertG1)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorDatum (..),
 )
import ZkFold.Cardano.UtxoAccumulator.Precompute qualified as Precompute

utxoAccumulatorDatumHash :: UtxoAccumulatorDatum -> BuiltinByteString
utxoAccumulatorDatumHash = blake2b_224 . serialiseData . toBuiltinData

datumFromHash :: [UtxoAccumulatorDatum] -> BuiltinByteString -> UtxoAccumulatorDatum
datumFromHash ds dh =
  fromJust $ find (\d -> utxoAccumulatorDatumHash d == dh) ds

utxoAccumulatorDatumsFromElements ::
  [BLS12_381_G1_Point] ->
  [UtxoAccumulatorDatum]
utxoAccumulatorDatumsFromElements gs =
  let lastAccPar :: UtxoAccumulatorDatum
      lastAccPar =
        let maybeCurrentGroupElement = Nothing
            nextDatumHash = ""
         in UtxoAccumulatorDatum {..}

      prec :: UtxoAccumulatorDatum -> BuiltinByteString -> UtxoAccumulatorDatum
      prec d g = d {nextDatumHash = utxoAccumulatorDatumHash d, maybeCurrentGroupElement = Just g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastAccPar]
        (map convertG1 gs)

addDatums :: [UtxoAccumulatorDatum]
addDatums = utxoAccumulatorDatumsFromElements Precompute.accumulationGroupElements

addDatumFromHash :: BuiltinByteString -> UtxoAccumulatorDatum
addDatumFromHash = datumFromHash addDatums

removeDatums :: [UtxoAccumulatorDatum]
removeDatums = utxoAccumulatorDatumsFromElements Precompute.distributionGroupElements

removeDatumFromHash :: BuiltinByteString -> UtxoAccumulatorDatum
removeDatumFromHash = datumFromHash removeDatums
