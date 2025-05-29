module ZkFold.Cardano.UtxoAccumulator.Datum where

import Data.Foldable (foldr', toList)
import PlutusLedgerApi.V3 (ToData (..))
import PlutusTx.Prelude (
  Bool (..),
  BuiltinByteString,
  Maybe (..),
  blake2b_224,
  head,
  ($),
  (.),
 )
import Prelude (map, (==))

import Data.List (find, init, last, (++))
import Data.Maybe (fromJust)
import PlutusTx.Builtins (serialiseData)
import ZkFold.Cardano.OffChain.BLS12_381 (convertG1)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorDatum (..),
 )
import ZkFold.Cardano.UtxoAccumulator.Precompute qualified as Precompute

utxoAccumulatorDatumHash :: UtxoAccumulatorDatum -> BuiltinByteString
utxoAccumulatorDatumHash = blake2b_224 . serialiseData . toBuiltinData

utxoAccumulatorDatumFromHash :: BuiltinByteString -> UtxoAccumulatorDatum
utxoAccumulatorDatumFromHash dh =
  fromJust
    $ find (\d -> utxoAccumulatorDatumHash d == dh)
    $ accumulationDatums
    ++ distributionDatums

distributionDatums :: [UtxoAccumulatorDatum]
distributionDatums =
  let lastDistPar :: UtxoAccumulatorDatum
      lastDistPar =
        let maybeNextDatumHash = Nothing
            canSwitch = False
            currentGroupElement = convertG1 $ last Precompute.distributionGroupElements
         in UtxoAccumulatorDatum {..}

      prec :: UtxoAccumulatorDatum -> BuiltinByteString -> UtxoAccumulatorDatum
      prec d g = d {maybeNextDatumHash = Just $ utxoAccumulatorDatumHash d, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastDistPar]
        (map convertG1 $ toList $ init Precompute.distributionGroupElements)

accumulationDatums :: [UtxoAccumulatorDatum]
accumulationDatums =
  let lastAccPar :: UtxoAccumulatorDatum
      lastAccPar =
        let maybeNextDatumHash = Nothing
            canSwitch = True
            currentGroupElement = convertG1 $ last Precompute.accumulationGroupElements
         in UtxoAccumulatorDatum {..}

      prec :: UtxoAccumulatorDatum -> BuiltinByteString -> UtxoAccumulatorDatum
      prec d g = d {maybeNextDatumHash = Just $ utxoAccumulatorDatumHash d, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastAccPar]
        (map convertG1 $ toList $ init Precompute.accumulationGroupElements)
