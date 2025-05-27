module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests,
) where

import Control.Monad.Reader (runReaderT)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V3 (Value)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Class (zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Api (addUtxo, initAccumulator, removeUtxo, switchAccumulator)
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolTreasuryAddress)
import ZkFold.Cardano.UtxoAccumulator.Types.Context (Context (..))

fundingRun :: User -> GYAddress -> GYValue -> Ctx -> IO ()
fundingRun treasury serverAddr serverFunds ctx = ctxRun ctx treasury $ do
  txBodyFund <- buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum serverAddr serverFunds
  signAndSubmitConfirmed_ txBodyFund

initAccumulatorRun :: GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Value -> Ctx -> IO (GYTxSkeleton 'PlutusV2, GYTxOutRef)
initAccumulatorRun serverAddr serverPaymentKey serverStakeKey v ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    ttRef <- head <$> utxoRefsAtAddress serverAddr
    txSkel <- initAccumulator v `runReaderT` Context ttRef v
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [serverPaymentKey]
    return (txSkel, ttRef)

addUtxoRun :: GYTxOutRef -> GYAddress -> GYAddress -> ScalarFieldOf BLS12_381_G1_Point -> Value -> User -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
addUtxoRun ttRef serverAddr recipient r v u ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey $ userPaymentSKey u)
  (AGYStakeSigningKey <$> userStakeSKey u)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    txSkel <- addUtxo serverAddr recipient r `runReaderT` Context ttRef v
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [AGYPaymentSigningKey $ userPaymentSKey u]
    return txSkel

switchAccumulatorRun :: GYTxOutRef -> GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Value -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
switchAccumulatorRun ttRef serverAddr serverPaymentKey serverStakeKey v ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    txSkel <- switchAccumulator `runReaderT` Context ttRef v
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [serverPaymentKey]
    return txSkel

removeUtxoRun :: GYTxOutRef -> GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Value -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
removeUtxoRun ttRef serverAddr serverPaymentKey serverStakeKey v ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    txSkel <- removeUtxo `runReaderT` Context ttRef v
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

          let accumulationValue :: Value
              accumulationValue = lovelaceValueOf 100_000_000

          -- Funding the server
          info $ "Server's address: " <> show serverAddr
          fundingRun user1 serverAddr (valueFromLovelace 2500_000_000) ctx
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Funded server. Server utxos: " <>) . show

          -- Initializing the accumulator
          (txInit, ttRef) <- initAccumulatorRun serverAddr serverPaymentKey serverStakeKey accumulationValue ctx
          info $ "Transaction: " <> show txInit
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Initialized accumulator. Server's utxos: " <>) . show

          -- Adding funds to the accumulator
          txAdd <- addUtxoRun ttRef serverAddr (userAddr user2) zero accumulationValue user1 ctx
          info $ "Transaction: " <> show txAdd

          -- Switching the accumulator to distribution mode
          txSwitch <- switchAccumulatorRun ttRef serverAddr serverPaymentKey serverStakeKey accumulationValue ctx
          info $ "Transaction: " <> show txSwitch

          -- Removing funds from the accumulator
          txRemove <- removeUtxoRun ttRef serverAddr serverPaymentKey serverStakeKey accumulationValue ctx
          info $ "Transaction: " <> show txRemove

          ctxRunQuery ctx (utxosAtAddress protocolTreasuryAddress Nothing)
            >>= info . ("End-to-end run completed. Treasury's utxos: " <>) . show
    ]
