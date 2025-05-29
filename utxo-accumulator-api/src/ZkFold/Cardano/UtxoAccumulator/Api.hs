module ZkFold.Cardano.UtxoAccumulator.Api (
  initAccumulator,
  addUtxo,
  removeUtxo,
) where

import Control.Monad.Reader (ask)
import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxSkeleton, addressFromPlutus', mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Api.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Constants
import ZkFold.Cardano.UtxoAccumulator.Transition (mkAddUtxo, mkRemoveUtxo)
import ZkFold.Cardano.UtxoAccumulator.Types (UtxoAccumulatorQueryMonad)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

initAccumulator ::
  UtxoAccumulatorQueryMonad m =>
  m (GYTxSkeleton 'PlutusV2)
initAccumulator = do
  Context {..} <- ask
  let t = valueSingleton (threadToken ctxThreadTokenRef) 1
  accAddr <- utxoAccumulatorAddress ctxAccumulationValue
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = ctxThreadTokenRef
        , gyTxInWitness = GYTxInWitnessKey
        }
      <> mustMint (GYBuildPlutusScript $ GYBuildPlutusScriptInlined $ threadTokenPolicy ctxThreadTokenRef) unitRedeemer threadTokenName 1
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = accAddr
          , gyTxOutValue = serverDeposit <> t
          , gyTxOutDatum = Just (utxoAccumulatorDatumInit, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

addUtxo ::
  UtxoAccumulatorQueryMonad m =>
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3, ScalarFieldOf BLS12_381_G1_Point)
addUtxo (addressToPlutus -> recipient) r = do
  Context {..} <- ask
  stateRef <- fromJust <$> getState (threadToken ctxThreadTokenRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer, h) = mkAddUtxo (fst $ fromJust gyTxOutDatum) recipient r
  addrAcc <- utxoAccumulatorAddress ctxAccumulationValue
  let txSkeleton =
        mustHaveInput
          GYTxIn
            { gyTxInTxOutRef = stateRef
            , gyTxInWitness = GYTxInWitnessScript (utxoAccumulatorBuildScript ctxAccumulatorScriptRef ctxAccumulationValue) (fst <$> gyTxOutDatum) redeemer
            }
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = addrAcc
              , gyTxOutValue = gyTxOutValue <> ctxAccumulationValue
              , gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum)
              , gyTxOutRefS = Nothing
              }
  return (txSkeleton, h)

removeUtxo ::
  UtxoAccumulatorQueryMonad m =>
  GYAddress ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  [ScalarFieldOf BLS12_381_G1_Point] ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
removeUtxo gyServer hs as (addressToPlutus -> recipient) r = do
  Context {..} <- ask
  stateRef <- fromJust <$> getState (threadToken ctxThreadTokenRef)
  GYTxOut {gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let (dat, redeemer) = mkRemoveUtxo (fst $ fromJust gyTxOutDatum) hs as recipient r
  addrAcc <- utxoAccumulatorAddress ctxAccumulationValue
  addrRecipient <- addressFromPlutus' recipient
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript (utxoAccumulatorBuildScript ctxAccumulatorScriptRef ctxAccumulationValue) (fst <$> gyTxOutDatum) redeemer
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue `valueMinus` ctxAccumulationValue
          , gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrRecipient
          , gyTxOutValue = ctxAccumulationValue
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
