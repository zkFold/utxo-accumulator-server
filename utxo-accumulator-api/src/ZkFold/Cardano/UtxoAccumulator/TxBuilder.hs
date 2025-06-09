module ZkFold.Cardano.UtxoAccumulator.TxBuilder where

import Control.Monad (unless)
import Data.Map (delete, insert, toList, (!))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Database (AccumulatorDataItem (..), AccumulatorDataKey (AccumulatorDataKey), UtxoDistributionTime, getUtxoAccumulatorData, putUtxoAccumulatorData, removeUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.IO (runBuilderWithConfig, runQueryWithConfig, runSignerWithConfig)
import ZkFold.Cardano.UtxoAccumulator.Sync (threadTokenRefFromSync, fullSyncFromConfig)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorHashWrapper, utxoAccumulatorAddressHash)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (addUtxo, initAccumulator, postScript, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

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
  UtxoAccumulatorCRS ->
  Config ->
  IO Config
initAccumulatorRun crs cfg@Config {..} = do
  removeUtxoAccumulatorData cfgDatabasePath
  runSignerWithConfig cfg $ do
    (txSkel, ref) <- initAccumulator crs cfgAddress cfgAccumulationValue
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [cfgPaymentKey]
    return $ cfg {cfgThreadTokenRefs = ref : cfgThreadTokenRefs}

addUtxoRun ::
  UtxoAccumulatorCRS ->
  Config ->
  GYAddress -> -- sender
  GYAddress -> -- recipient
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  UtxoDistributionTime ->
  IO GYTx
addUtxoRun crs cfg@Config {..} sender recipient l r distTime = do
  -- Update the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  ref <- fromJust <$> threadTokenRefFromSync cfg
  let key = AccumulatorDataKey recipient l
      item = AccumulatorDataItem r distTime ref
  putUtxoAccumulatorData cfgDatabasePath $ insert key item m

  -- Build the transaction skeleton
  runBuilderWithConfig cfg sender $ do
    txSkel <- addUtxo crs cfgAccumulationValue (fromJust cfgMaybeScriptRef) ref cfgAddress recipient l r
    unsignedTx <$> buildTxBody txSkel

removeUtxoRun :: UtxoAccumulatorCRS -> Config -> Bool -> IO ()
removeUtxoRun crs cfg@Config {..} removeNoDate = do
  -- Get the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  unless (null m) $ do
    now <- getPOSIXTime
    cache <- fullSyncFromConfig cfg

    -- Find UTxOs eligible for removal
    let eligible =
          [ (recipient, l, r, ttRef)
          | (AccumulatorDataKey recipient l, AccumulatorDataItem r mDistTime ttRef) <- toList m
          , maybe removeNoDate (<= now) mDistTime
          , utxoAccumulatorAddressHash (addressToPlutus recipient) l `notElem` fst (cache ! ttRef)
          ]
    case eligible of
      [] -> return () -- No eligible UTxOs to remove at this time
      ((recipient, l, r, ttRef) : _) -> do
        let (hs, as) = case cfgMaestroToken of
              "" -> ([utxoAccumulatorHashWrapper (addressToPlutus recipient) l r], [])
              _ -> cache ! ttRef

        -- Build, sign, and submit the transaction
        runSignerWithConfig cfg $ do
          txSkel <- removeUtxo crs cfgAccumulationValue (fromJust cfgMaybeScriptRef) ttRef hs as recipient l r
          txBody <- buildTxBody txSkel
          submitTxBodyConfirmed_ txBody [cfgPaymentKey]

        -- Update the database by removing the UTXO
        putUtxoAccumulatorData cfgDatabasePath $ delete (AccumulatorDataKey recipient l) m
        -- Repeat until all eligible UTxOs are removed
        removeUtxoRun crs cfg removeNoDate
