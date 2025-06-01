module ZkFold.Cardano.UtxoAccumulator.TxBuilder where

import Control.Monad (unless)
import Data.Map (insert)
import Data.Maybe (fromJust)
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Constants (threadToken)
import ZkFold.Cardano.UtxoAccumulator.Database (getUtxoAccumulatorData, putUtxoAccumulatorData, removeUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.Sync (findUnusedTransactionData, fullSync)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorHashWrapper)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (addUtxo, initAccumulator, postScript, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Utils (getOutput, getState)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))

runQueryWithConfig :: Config -> GYTxQueryMonadIO a -> IO a
runQueryWithConfig cfg =
  runGYTxQueryMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)

runBuilderWithConfig :: Config -> GYAddress -> GYTxBuilderMonadIO a -> IO a
runBuilderWithConfig cfg addr =
  runGYTxBuilderMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)
    [addr]
    addr
    Nothing

runSignerWithConfig :: Config -> GYTxMonadIO a -> IO a
runSignerWithConfig cfg =
  runGYTxMonadIO
    (cfgNetworkId cfg)
    (cfgProviders cfg)
    (cfgPaymentKey cfg)
    (cfgStakeKey cfg)
    [cfgAddress cfg]
    (cfgAddress cfg)
    Nothing

postScriptRun ::
  Config ->
  IO Config
postScriptRun cfg@Config {..} = do
  -- Build the transaction skeleton
  txSkel <- runQueryWithConfig cfg $ postScript cfgAccumulationValue

  -- Sign and submit the transaction
  txId <- runSignerWithConfig cfg $ do
    txBody <- buildTxBody txSkel
    signAndSubmitConfirmed txBody
  return $ cfg {cfgMaybeScriptRef = Just $ txOutRefFromTuple (txId, 0)}

initAccumulatorRun ::
  Config ->
  IO Config
initAccumulatorRun cfg@Config {..} = do
  removeUtxoAccumulatorData cfgDatabasePath
  runSignerWithConfig cfg $ do
    (txSkel, ref) <- initAccumulator cfgAddress cfgAccumulationValue
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [cfgPaymentKey]
    return $ cfg {cfgMaybeThreadTokenRef = Just ref}

addUtxoRun ::
  Config ->
  GYAddress ->
  GYAddress ->
  ScalarFieldOf BLS12_381_G1_Point ->
  IO GYTx
addUtxoRun cfg@Config {..} sender recipient r = do
  -- Update the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  putUtxoAccumulatorData cfgDatabasePath $ insert recipient r m

  -- Build the transaction skeleton
  runBuilderWithConfig cfg sender $ do
    txSkel <- addUtxo cfgAccumulationValue (fromJust cfgMaybeScriptRef) (fromJust cfgMaybeThreadTokenRef) recipient r
    unsignedTx <$> buildTxBody txSkel

removeUtxoRun :: Config -> IO ()
removeUtxoRun cfg@Config {..} = do
  -- Get the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  unless (null m) $ do
    (nId, txOut) <- runQueryWithConfig cfg $ do
      nId <- networkId
      stateRef <- fromJust <$> getState (threadToken $ fromJust cfgMaybeThreadTokenRef)
      (nId,) . fromJust <$> getOutput stateRef
    (_, as) <- fullSync nId txOut
    let (recipient, r) = fromJust $ findUnusedTransactionData m as
        hs = [utxoAccumulatorHashWrapper (addressToPlutus recipient) r]

    -- Build, sign, and submit the transaction
    runSignerWithConfig cfg $ do
      txSkel <- removeUtxo cfgAccumulationValue (fromJust cfgMaybeScriptRef) (fromJust cfgMaybeThreadTokenRef) cfgAddress hs as recipient r
      txBody <- buildTxBody txSkel
      submitTxBodyConfirmed_ txBody [cfgPaymentKey]

    -- Repeat until all UTXOs are removed
    removeUtxoRun cfg
