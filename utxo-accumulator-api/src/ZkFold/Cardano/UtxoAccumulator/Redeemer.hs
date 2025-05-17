module ZkFold.Cardano.UtxoAccumulator.Redeemer where

import PlutusLedgerApi.V3 (Address (..), ToData (..))
import PlutusTx.Builtins (ByteOrder (..), serialiseData)
import PlutusTx.Prelude (
  blake2b_224,
  byteStringToInteger,
  ($),
 )
import Prelude (Integer, snd)

import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.OffChain.BLS12_381 (convertZp)
import ZkFold.Cardano.OffChain.Plonkup (mkProof)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorRedeemer (..),
 )
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N)
import ZkFold.Symbolic.Examples.UtxoAccumulator (
  utxoAccumulatorHash,
  utxoAccumulatorProve,
 )

mkAddUtxo ::
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (UtxoAccumulatorRedeemer, Integer)
mkAddUtxo addr r =
  let a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
      h = convertZp $ utxoAccumulatorHash (toZp a) r
   in (AddUtxo h, h)

mkRemoveUtxo ::
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  Address ->
  ScalarFieldOf BLS12_381_G1_Point ->
  (UtxoAccumulatorRedeemer, Integer)
mkRemoveUtxo hs as addr r =
  let a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr
      proof = mkProof $ snd $ utxoAccumulatorProve @N @M hs as (toZp a) r
   in (RemoveUtxo addr proof, a)
