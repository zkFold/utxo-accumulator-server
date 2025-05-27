module ZkFold.Cardano.UtxoAccumulator.Api (
  initAccumulator,
  addUtxo,
  switchAccumulator,
  removeUtxo,
) where

import Control.Monad.Reader (ask)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import GeniusYield.TxBuilder (GYTxSkeleton, addressFromPlutus', mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types
import PlutusLedgerApi.V3 (Value)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorParameters (..), UtxoAccumulatorRedeemer (..), utxoAccumulatorCompiled)
import ZkFold.Cardano.UtxoAccumulator.Api.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolFee, protocolTreasuryAddress, serverDeposit, serverFee, threadToken, threadTokenName, threadTokenPolicy, utxoAccumulatorSetupBytesInit)
import ZkFold.Cardano.UtxoAccumulator.Datum (updateDatum)
import ZkFold.Cardano.UtxoAccumulator.Redeemer (mkAddUtxo, mkRemoveUtxo)
import ZkFold.Cardano.UtxoAccumulator.ScriptParameters (accumulationParameters, utxoAccumulatorAddress, utxoAccumulatorParametersFromAddress)
import ZkFold.Cardano.UtxoAccumulator.Types (UtxoAccumulatorQueryMonad)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

initAccumulator ::
  UtxoAccumulatorQueryMonad m =>
  Value ->
  m (GYTxSkeleton 'PlutusV2)
initAccumulator v = do
  Context {..} <- ask
  let t = valueSingleton (threadToken ctxThreadTokenRef) 1
      params = head $ accumulationParameters v
  accAddr <- addressFromPlutus' $ utxoAccumulatorAddress params
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
          , gyTxOutDatum = Just (datumFromPlutusData utxoAccumulatorSetupBytesInit, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

addUtxo ::
  UtxoAccumulatorQueryMonad m =>
  GYAddress ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  m (GYTxSkeleton 'PlutusV3)
addUtxo gyServer (addressToPlutus -> recipient) r = do
  Context {..} <- ask
  stateRef <- fromJust <$> getState (threadToken ctxThreadTokenRef)
  GYTxOut {gyTxOutAddress, gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let params@UtxoAccumulatorParameters {maybeNextAddress, currentGroupElement} =
        fromJust $
          utxoAccumulatorParametersFromAddress ctxAccumulationValue (addressToPlutus gyTxOutAddress)
      (redeemer, h) = mkAddUtxo recipient r

      script :: GYBuildPlutusScript 'PlutusV3
      script = GYInScript @_ @PlutusV3 $ scriptFromPlutus $ utxoAccumulatorCompiled params
  addrAcc <- addressFromPlutus' $ fromJust maybeNextAddress
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript script (fst <$> gyTxOutDatum) (redeemerFromPlutusData redeemer)
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue <> fromRight (error "Parsing value failed.") (valueFromPlutus ctxAccumulationValue)
          , gyTxOutDatum = Just (updateDatum (fst $ fromJust gyTxOutDatum) h currentGroupElement, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = gyServer
          , gyTxOutValue = fromRight (error "Parsing value failed.") (valueFromPlutus serverFee)
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = protocolTreasuryAddress
          , gyTxOutValue = fromRight (error "Parsing value failed.") (valueFromPlutus protocolFee)
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }

switchAccumulator ::
  UtxoAccumulatorQueryMonad m =>
  m (GYTxSkeleton 'PlutusV3)
switchAccumulator = do
  Context {..} <- ask
  stateRef <- fromJust <$> getState (threadToken ctxThreadTokenRef)
  GYTxOut {gyTxOutAddress, gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let params@UtxoAccumulatorParameters {..} =
        fromJust $
          utxoAccumulatorParametersFromAddress ctxAccumulationValue (addressToPlutus gyTxOutAddress)

      script :: GYBuildPlutusScript 'PlutusV3
      script = GYInScript @_ @PlutusV3 $ scriptFromPlutus $ utxoAccumulatorCompiled params
  addrAcc <- addressFromPlutus' $ fromJust maybeNextAddress
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript script (fst <$> gyTxOutDatum) (redeemerFromPlutusData Switch)
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue
          , gyTxOutDatum = Just (updateDatum (fst $ fromJust gyTxOutDatum) 1 switchGroupElement, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }

removeUtxo ::
  UtxoAccumulatorQueryMonad m =>
  m (GYTxSkeleton 'PlutusV3)
removeUtxo = do
  Context {..} <- ask
  hs <- undefined
  as <- undefined
  recipient <- undefined
  r <- undefined
  stateRef <- fromJust <$> getState (threadToken ctxThreadTokenRef)
  GYTxOut {gyTxOutAddress, gyTxOutValue, gyTxOutDatum} <- fromJust <$> getOutput stateRef
  let params@UtxoAccumulatorParameters {..} =
        fromJust $
          utxoAccumulatorParametersFromAddress ctxAccumulationValue (addressToPlutus gyTxOutAddress)
      (redeemer, a) = mkRemoveUtxo hs as recipient r

      script :: GYBuildPlutusScript 'PlutusV3
      script = GYInScript @_ @PlutusV3 $ scriptFromPlutus $ utxoAccumulatorCompiled params
  addrAcc <- addressFromPlutus' $ fromJust maybeNextAddress
  addrRecipient <- addressFromPlutus' recipient
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = stateRef
        , gyTxInWitness = GYTxInWitnessScript script (fst <$> gyTxOutDatum) (redeemerFromPlutusData redeemer)
        }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrAcc
          , gyTxOutValue = gyTxOutValue `valueMinus` fromRight (error "Parsing value failed.") (valueFromPlutus ctxAccumulationValue)
          , gyTxOutDatum = Just (updateDatum (fst $ fromJust gyTxOutDatum) a currentGroupElement, GYTxOutUseInlineDatum)
          , gyTxOutRefS = Nothing
          }
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = addrRecipient
          , gyTxOutValue = fromRight (error "Parsing value failed.") (valueFromPlutus ctxAccumulationValue)
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
