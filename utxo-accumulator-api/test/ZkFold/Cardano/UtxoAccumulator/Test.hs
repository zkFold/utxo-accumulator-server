module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests,
) where

import Control.Monad.Reader (runReaderT)
import Data.Map (insert)
import Data.Maybe (fromJust)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UtxoAccumulator.Api (addUtxo, initAccumulator, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.Api.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolTreasuryAddress, scriptParkingAddress, threadToken, utxoAccumulatorAddress, utxoAccumulatorScript)
import ZkFold.Cardano.UtxoAccumulator.Database (getUtxoAccumulatorData, putUtxoAccumulatorData, removeUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.Sync (findUnusedTransactionData, fullSync)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorHashWrapper)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

fundingRun :: User -> GYAddress -> GYValue -> GYValue -> Ctx -> IO GYTxOutRef
fundingRun treasury serverAddr serverFunds v ctx = ctxRun ctx treasury $ do
  parkingAddr <- scriptParkingAddress
  txBodyFund <-
    buildTxBody $
      mustHaveOutput (mkGYTxOutNoDatum serverAddr serverFunds)
        <> mustHaveOutput
          GYTxOut
            { gyTxOutAddress = parkingAddr
            , gyTxOutValue = valueFromLovelace 20_000_000
            , gyTxOutDatum = Nothing
            , gyTxOutRefS = Just $ GYPlutusScript $ utxoAccumulatorScript v
            }
  txId <- signAndSubmitConfirmed txBodyFund
  return $ txOutRefFromTuple (txId, 1)

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

utxoAccumulatorTests :: Setup -> TestTree
utxoAccumulatorTests setup =
  testGroup
    "utxo-accumulator-tests"
    [ testCaseSteps "able to initialize the server, initialize the accumulator, and perform accumulation-distribution process" $
        \info -> withSetup info setup $ \ctx -> do
          let nid = ctxNetworkId ctx
              user1 = ctxUserF ctx
              user2 = ctxUser2 ctx

          -- Generating server keys
          serverPaymentKey :: GYSigningKey 'GYKeyRolePayment <- generateSigningKey
          serverStakeKey :: GYSigningKey 'GYKeyRoleStaking <- generateSigningKey
          let serverPaymentKeyHash = serverPaymentKey & getVerificationKey & verificationKeyHash
              serverStakeKeyHash = serverStakeKey & getVerificationKey & verificationKeyHash
              serverAddr = addressFromCredential nid (GYPaymentCredentialByKey serverPaymentKeyHash) (Just $ GYStakeCredentialByKey serverStakeKeyHash)

          let accumulationValue :: GYValue
              accumulationValue = valueFromLovelace 100_000_000

          let fp = "txs.json"

          -- writeFileJSON "utxo-accumulator-api/accumulation-group-elements.json" (accumulationGroupElements @N @M)
          -- writeFileJSON "utxo-accumulator-api/distribution-group-elements.json" (distributionGroupElements @N @M)

          info $ "Protocol script size: " <> show (scriptSize $ GYPlutusScript $ utxoAccumulatorScript accumulationValue)
          ctxRunQuery ctx (utxoAccumulatorAddress accumulationValue)
            >>= info . ("Protocol script address: " <>) . show

          -- Funding the server
          info $ "Server's address: " <> show serverAddr
          scriptRef <- fundingRun user1 serverAddr (valueFromLovelace 2500_000_000) accumulationValue ctx
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Funded server. Server utxos: " <>) . show

          -- Initializing the accumulator
          removeUtxoAccumulatorData fp
          (txInit, context) <- initAccumulatorRun scriptRef serverAddr serverPaymentKey serverStakeKey accumulationValue ctx
          info $ "Transaction: " <> show txInit
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Initialized accumulator. Server's utxos: " <>) . show

          -- Adding funds to the accumulator
          let r = toZp 42
          txAdd <- addUtxoRun fp context (userAddr user2) r user1 ctx
          info $ "Transaction: " <> show txAdd

          -- Removing funds from the accumulator
          txRemove <- removeUtxoRun fp context serverAddr serverPaymentKey serverStakeKey ctx
          info $ "Transaction: " <> show txRemove

          ctxRunQuery ctx (utxosAtAddress protocolTreasuryAddress Nothing)
            >>= info . ("End-to-end run completed. Treasury's utxos: " <>) . show
    ]
