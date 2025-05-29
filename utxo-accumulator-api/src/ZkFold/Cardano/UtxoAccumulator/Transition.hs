module ZkFold.Cardano.UtxoAccumulator.Transition where

import PlutusLedgerApi.V3 (Address (..), ToData (..), Datum (..), UnsafeFromData (..))
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (
  blake2b_224,
  byteStringToInteger,
  ($), head,
 )
import Prelude (snd)

import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.OffChain.BLS12_381 (convertZp, convertG1)
import ZkFold.Cardano.OffChain.Plonkup (mkProof)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorRedeemer (..), UtxoAccumulatorDatum (..)
 )
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N)
import ZkFold.Symbolic.Examples.UtxoAccumulator (
  utxoAccumulatorHash,
  utxoAccumulatorProve,
 )
import GeniusYield.Types (GYDatum, datumToPlutus, datumFromPlutusData, GYRedeemer, redeemerFromPlutusData)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import Data.Maybe (fromJust)
import ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)
import ZkFold.Cardano.UtxoAccumulator.Datum (utxoAccumulatorDatumFromHash, distributionDatums)
import qualified ZkFold.Cardano.UtxoAccumulator.Precompute as Precompute

mkAddUtxo ::
  GYDatum ->
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (GYDatum, GYRedeemer)
mkAddUtxo dat addr r =
  let Datum d = datumToPlutus dat
      (UtxoAccumulatorDatum {..}, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)

      a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
      h = convertZp $ utxoAccumulatorHash (toZp a) r

      d' = utxoAccumulatorDatumFromHash (fromJust maybeNextDatumHash)
      setup' = updateSetupBytes setup h currentGroupElement
      dat' = datumFromPlutusData $ toBuiltinData (d', setup')
   in (dat', redeemerFromPlutusData $ AddUtxo h d')

mkSwitchAccumulator ::
  GYDatum ->
  (GYDatum, GYRedeemer)
mkSwitchAccumulator dat =
  let Datum d = datumToPlutus dat
      (_, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)

      d' = head distributionDatums
      setup' = updateSetupBytes setup 1 $ convertG1 Precompute.switchGroupElement
      dat' = datumFromPlutusData $ toBuiltinData (d', setup')
   in (dat', redeemerFromPlutusData $ Switch d')

mkRemoveUtxo ::
  GYDatum ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (GYDatum, GYRedeemer)
mkRemoveUtxo dat hs as addr r =
  let Datum d = datumToPlutus dat
      (UtxoAccumulatorDatum {..}, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)

      a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
      proof = mkProof $ snd $ utxoAccumulatorProve @N @M hs as (toZp a) r

      d' = utxoAccumulatorDatumFromHash (fromJust maybeNextDatumHash)
      setup' = updateSetupBytes setup a currentGroupElement
      dat' = datumFromPlutusData $ toBuiltinData (d', setup')
   in (dat', redeemerFromPlutusData $ RemoveUtxo addr proof d')
