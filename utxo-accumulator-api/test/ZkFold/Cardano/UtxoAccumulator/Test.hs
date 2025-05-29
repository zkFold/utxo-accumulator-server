module ZkFold.Cardano.UtxoAccumulator.Test (
  utxoAccumulatorTests,
) where

import Control.Monad.Reader (runReaderT)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Class (zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Api (addUtxo, initAccumulator, removeUtxo, switchAccumulator)
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolTreasuryAddress, scriptParkingAddress, utxoAccumulatorAddress, utxoAccumulatorScript)
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

addUtxoRun :: Context -> GYAddress -> GYAddress -> ScalarFieldOf BLS12_381_G1_Point -> User -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
addUtxoRun context serverAddr recipient r u ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey $ userPaymentSKey u)
  (AGYStakeSigningKey <$> userStakeSKey u)
  [userAddr u]
  (userAddr u)
  Nothing
  $ do
    txSkel <- addUtxo serverAddr recipient r `runReaderT` context
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [AGYPaymentSigningKey $ userPaymentSKey u]
    return txSkel

switchAccumulatorRun :: Context -> GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
switchAccumulatorRun context serverAddr serverPaymentKey serverStakeKey ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    txSkel <- switchAccumulator `runReaderT` context
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [serverPaymentKey]
    return txSkel

removeUtxoRun :: Context -> GYAddress -> GYPaymentSigningKey -> GYStakeSigningKey -> Ctx -> IO (GYTxSkeleton 'PlutusV3)
removeUtxoRun context serverAddr serverPaymentKey serverStakeKey ctx = runGYTxMonadIO
  (ctxNetworkId ctx)
  (ctxProviders ctx)
  (AGYPaymentSigningKey serverPaymentKey)
  (Just $ AGYStakeSigningKey serverStakeKey)
  [serverAddr]
  serverAddr
  Nothing
  $ do
    txSkel <- removeUtxo `runReaderT` context
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

          info $ "Protocol script size: " <> show (scriptSize $ GYPlutusScript $ utxoAccumulatorScript accumulationValue)

          -- Funding the server
          info $ "Server's address: " <> show serverAddr
          scriptRef <- fundingRun user1 serverAddr (valueFromLovelace 2500_000_000) accumulationValue ctx
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Funded server. Server utxos: " <>) . show

          -- Initializing the accumulator
          (txInit, context) <- initAccumulatorRun scriptRef serverAddr serverPaymentKey serverStakeKey accumulationValue ctx
          info $ "Transaction: " <> show txInit
          ctxRunQuery ctx (utxosAtAddress serverAddr Nothing)
            >>= info . ("Initialized accumulator. Server's utxos: " <>) . show

          -- Adding funds to the accumulator
          txAdd <- addUtxoRun context serverAddr (userAddr user2) zero user1 ctx
          info $ "Transaction: " <> show txAdd

          -- Switching the accumulator to distribution mode
          txSwitch <- switchAccumulatorRun context serverAddr serverPaymentKey serverStakeKey ctx
          info $ "Transaction: " <> show txSwitch

          -- Removing funds from the accumulator
          txRemove <- removeUtxoRun context serverAddr serverPaymentKey serverStakeKey ctx
          info $ "Transaction: " <> show txRemove

          ctxRunQuery ctx (utxosAtAddress protocolTreasuryAddress Nothing)
            >>= info . ("End-to-end run completed. Treasury's utxos: " <>) . show
    ]
