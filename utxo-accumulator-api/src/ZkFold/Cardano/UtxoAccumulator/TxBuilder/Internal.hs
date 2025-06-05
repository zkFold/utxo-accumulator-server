module ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (
  postScript,
  initAccumulator,
  addUtxo,
  removeUtxo,
) where

import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxQueryMonad, GYTxSkeleton, addressFromPlutus', mustHaveInput, mustHaveOutput, mustMint, utxoRefsAtAddress)
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Constants
import ZkFold.Cardano.UtxoAccumulator.Transition (mkAddUtxo, mkRemoveUtxo)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Utils (getOutput, getState)

postScript ::
  GYTxQueryMonad m =>
  GYValue ->
  m (GYTxSkeleton 'PlutusV2)
postScript accumulationValue = do
  parkingAddr <- scriptParkingAddress
  return $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = parkingAddr
        , gyTxOutValue = mempty
        , gyTxOutDatum = Nothing
        , gyTxOutRefS = Just $ GYPlutusScript $ utxoAccumulatorScript accumulationValue
        }

initAccumulator ::
  GYTxQueryMonad m =>
  GYAddress ->
  GYValue ->
  m (GYTxSkeleton 'PlutusV2, GYTxOutRef)
initAccumulator serverAddr accumulationValue = do
  ref <- head <$> utxoRefsAtAddress serverAddr
  let t = valueSingleton (threadToken ref) 1
  accAddr <- utxoAccumulatorAddress accumulationValue
  return $
    (,ref) $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = ref
          , gyTxInWitness = GYTxInWitnessKey
          }
        <> mustMint (GYBuildPlutusScript $ GYBuildPlutusScriptInlined $ threadTokenPolicy ref) unitRedeemer threadTokenName 1
        <> mustHaveOutput
          GYTxOut
            { gyTxOutAddress = accAddr
            , gyTxOutValue = t
            , gyTxOutDatum = Just (utxoAccumulatorDatumInit, GYTxOutUseInlineDatum)
            , gyTxOutRefS = Nothing
            }

addUtxo ::
  GYTxQueryMonad m =>
  GYValue ->
  GYTxOutRef ->
  GYTxOutRef ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
addUtxo accumulationValue scriptRef ttRef (addressToPlutus -> recipient) r = do
  stateRef <- fromJust <$> getState (threadToken ttRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer) = mkAddUtxo (fst $ fromJust gyTxOutDatum) recipient r
  addrAcc <- utxoAccumulatorAddress accumulationValue
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript (utxoAccumulatorBuildScript scriptRef accumulationValue) (fst <$> gyTxOutDatum) redeemer
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue <> accumulationValue
          , gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

removeUtxo ::
  GYTxQueryMonad m =>
  GYValue ->
  GYTxOutRef ->
  GYTxOutRef ->
  GYAddress ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
removeUtxo accumulationValue scriptRef ttRef gyServer hs as (addressToPlutus -> recipient) r = do
  stateRef <- fromJust <$> getState (threadToken ttRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer) = mkRemoveUtxo (fst $ fromJust gyTxOutDatum) hs as recipient r
  addrAcc <- utxoAccumulatorAddress accumulationValue
  addrRecipient <- addressFromPlutus' recipient
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript (utxoAccumulatorBuildScript scriptRef accumulationValue) (fst <$> gyTxOutDatum) redeemer
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue `valueMinus` accumulationValue
          , gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrRecipient
          , gyTxOutValue = accumulationValue
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = gyServer
          , gyTxOutValue = serverFee
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = protocolTreasuryAddress
          , gyTxOutValue = protocolFee
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
