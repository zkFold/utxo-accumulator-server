module ZkFold.Cardano.UtxoAccumulator.TxBuilder where

import Control.Monad.Reader (runReaderT)
import Data.Map (insert)
import Data.Maybe (fromJust)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (addUtxo, initAccumulator, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Constants (threadToken)
import ZkFold.Cardano.UtxoAccumulator.Database (getUtxoAccumulatorData, putUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.Sync (findUnusedTransactionData, fullSync)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorHashWrapper)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

initAccumulatorRun :: GYTxOutRef -> GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> GYValue -> Ctx -> IO (GYTxSkeleton 'PlutusV2, Context)
initAccumulatorRun ctxAccumulatorScriptRef serverAddr serverPaymentKey serverStakeKey ctxAccumulationValue ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    ctxThreadTokenRef <- head <$> utxoRefsAtAddress serverAddr
    let context = Context {..}
    txSkel <- initAccumulator `runReaderT` Context {..}
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [serverPaymentKey]
    return (txSkel, context)

addUtxoRun :: FilePath -> Context -> GYAddress -> ScalarFieldOf BLS12_381_G1_Point -> User -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
addUtxoRun fp context recipient r u ctx = do
  m <- getUtxoAccumulatorData fp
  putUtxoAccumulatorData fp $ insert recipient r m
  runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey $ userPaymentSKey u)
    (AGYStakeSigningKey <$> userStakeSKey u)
    [userAddr u]
    (userAddr u)
    Nothing
    $ do
      txSkel <- addUtxo recipient r `runReaderT` context
      txBody <- buildTxBody txSkel
      submitTxBodyConfirmed_ txBody [AGYPaymentSigningKey $ userPaymentSKey u]
      return txSkel

removeUtxoRun ::
  FilePath ->
  Context ->
  GYAddress ->
  GYPaymentSigningKey ->
  GYStakeSigningKey ->
  Ctx ->
  IO (GYTxSkeleton 'PlutusV3)
removeUtxoRun fp context serverAddr serverPaymentKey serverStakeKey ctx = do
  m <- getUtxoAccumulatorData fp
  (nId, txOut) <- runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey serverPaymentKey)
    (Just $ AGYStakeSigningKey serverStakeKey)
    [serverAddr]
    serverAddr
    Nothing
    $ do
      nId <- networkId
      stateRef <- fromJust <$> getState (threadToken $ ctxThreadTokenRef context)
      (nId,) . fromJust <$> getOutput stateRef
  (_, as) <- fullSync nId txOut
  let (recipient, r) = fromJust $ findUnusedTransactionData m as
      hs = [utxoAccumulatorHashWrapper (addressToPlutus recipient) r]
  runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey serverPaymentKey)
    (Just $ AGYStakeSigningKey serverStakeKey)
    [serverAddr]
    serverAddr
    Nothing
    $ do
      txSkel <- removeUtxo serverAddr hs as recipient r `runReaderT` context
      txBody <- buildTxBody txSkel
      submitTxBodyConfirmed_ txBody [serverPaymentKey]
      return txSkel
