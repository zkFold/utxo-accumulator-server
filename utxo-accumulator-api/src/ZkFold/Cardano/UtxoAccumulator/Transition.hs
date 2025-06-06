module ZkFold.Cardano.UtxoAccumulator.Transition where

import PlutusLedgerApi.V3 (Address (..), Datum (..), ToData (..), UnsafeFromData (..))
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (
  blake2b_224,
  byteStringToInteger,
  ($),
 )
import Prelude ((++))

import Data.Maybe (fromJust)
import GeniusYield.Types (GYDatum, GYRedeemer, datumFromPlutusData, datumToPlutus, redeemerFromPlutusData)
import ZkFold.Algebra.Class (zero, (-!))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Algebra.Number (value)
import ZkFold.Cardano.OffChain.BLS12_381 (convertZp)
import ZkFold.Cardano.OffChain.Plonkup (mkProof)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorDatum (..),
  UtxoAccumulatorRedeemer (..),
 )
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N)
import ZkFold.Cardano.UtxoAccumulator.Datum (addDatumFromHash, removeDatumFromHash)
import ZkFold.Prelude (length, replicate)
import ZkFold.Symbolic.Examples.UtxoAccumulator (
  UtxoAccumulatorCRS,
  utxoAccumulatorHash,
  utxoAccumulatorProve,
 )

utxoAccumulatorHashWrapper ::
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point
utxoAccumulatorHashWrapper addr l r =
  let a =
        toZp
          $ byteStringToInteger BigEndian
          $ blake2b_224
          $ serialiseData
          $ toBuiltinData
            (addr, convertZp l)
   in utxoAccumulatorHash a r

mkAddUtxo ::
  GYDatum ->
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (GYDatum, GYRedeemer)
mkAddUtxo dat addr l r =
  let Datum d = datumToPlutus dat
      (UtxoAccumulatorDatum {..}, datumRemove, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, UtxoAccumulatorDatum, SetupBytes)

      h = convertZp $ utxoAccumulatorHashWrapper addr l r

      d' = addDatumFromHash nextDatumHash
      setup' = updateSetupBytes setup h $ fromJust maybeCurrentGroupElement
      dat' = datumFromPlutusData $ toBuiltinData (d', datumRemove, setup')
   in (dat', redeemerFromPlutusData $ AddUtxo h d')

mkRemoveUtxo ::
  UtxoAccumulatorCRS ->
  GYDatum ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (GYDatum, GYRedeemer)
mkRemoveUtxo crs dat hs as addr l r =
  let Datum d = datumToPlutus dat
      (datumAdd, UtxoAccumulatorDatum {..}, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, UtxoAccumulatorDatum, SetupBytes)

      a' =
        byteStringToInteger BigEndian
          $ blake2b_224
          $ serialiseData
          $ toBuiltinData
            (addr, convertZp l)
      a = toZp a'

      n = value @N
      hs' = hs ++ replicate (n -! length hs) zero
      as' = as ++ replicate (n -! length as) zero

      (_, proof) = utxoAccumulatorProve @N @M crs hs' as' a r

      d' = removeDatumFromHash nextDatumHash
      setup' = updateSetupBytes setup a' $ fromJust maybeCurrentGroupElement
      dat' = datumFromPlutusData $ toBuiltinData (datumAdd, d', setup')
   in (dat', redeemerFromPlutusData $ RemoveUtxo addr (convertZp l) (mkProof proof) d')
