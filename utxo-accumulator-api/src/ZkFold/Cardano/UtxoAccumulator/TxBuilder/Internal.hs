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
import ZkFold.Cardano.UtxoAccumulator.Sync (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Transition (mkAddUtxo, mkRemoveUtxo)
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

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
  UtxoAccumulatorCRS ->
  GYAddress ->
  GYValue ->
  m (GYTxSkeleton 'PlutusV2, GYTxOutRef)
initAccumulator crs serverAddr accumulationValue = do
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
            , gyTxOutDatum = Just (utxoAccumulatorDatumInit crs, GYTxOutUseInlineDatum)
            , gyTxOutRefS = Nothing
            }

addUtxo ::
  GYTxQueryMonad m =>
  UtxoAccumulatorCRS ->
  GYValue ->
  GYTxOutRef ->
  GYTxOutRef ->
  GYAddress -> -- server address
  GYAddress -> -- recipient
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
addUtxo crs accumulationValue scriptRef ttRef gyServer recipient l r = do
  stateRef <- fromJust <$> getState (threadToken ttRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer) = mkAddUtxo crs (fst $ fromJust gyTxOutDatum) (addressToPlutus recipient) l r
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
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = gyServer
          , gyTxOutValue = serverFee accumulationValue
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = protocolTreasuryAddress
          , gyTxOutValue = protocolFee accumulationValue
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }

removeUtxo ::
  GYTxQueryMonad m =>
  UtxoAccumulatorCRS ->
  GYValue ->
  GYTxOutRef ->
  GYTxOutRef ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
removeUtxo crs accumulationValue scriptRef ttRef hs as (addressToPlutus -> recipient) l r = do
  stateRef <- fromJust <$> getState (threadToken ttRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer) = mkRemoveUtxo crs (fst $ fromJust gyTxOutDatum) hs as recipient l r
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
