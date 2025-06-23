module ZkFold.Cardano.UtxoAccumulator.TxBuilder where

import Control.Monad (unless)
import Data.Map (delete, filter, insert, keys, toList, (!))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Cardano.UtxoAccumulator.Database (AccumulatorDataItem (..), AccumulatorDataKey (AccumulatorDataKey), UtxoDistributionTime, getUtxoAccumulatorData, putUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.IO (runBuilderWithConfig, runQueryWithConfig, runSignerWithConfig)
import ZkFold.Cardano.UtxoAccumulator.Sync (fullSyncFromConfig)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorAddressHash, utxoAccumulatorHashWrapper)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (addUtxo, initAccumulator, postScript, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)
import Prelude hiding (filter)

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
initAccumulatorRun crs cfg@Config {..} = runSignerWithConfig cfg $ do
  (txSkel, ref) <- initAccumulator crs cfgAddress cfgAccumulationValue
  txBody <- buildTxBody txSkel
  submitTxBodyConfirmed_ txBody [cfgPaymentKey]
  return $ cfg {cfgThreadTokenRefs = ref : cfgThreadTokenRefs}

addUtxoRun ::
  UtxoAccumulatorCRS ->
  Config ->
  GYTxOutRef -> -- current thread token reference
  GYAddress -> -- sender
  GYAddress -> -- recipient
  ScalarFieldOf BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  UtxoDistributionTime ->
  IO GYTx
addUtxoRun crs cfg@Config {..} ref sender recipient l r distTime = do
  -- Update the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  let key = AccumulatorDataKey recipient l
      item = AccumulatorDataItem r distTime ref
  putUtxoAccumulatorData cfgDatabasePath $ insert key item m

  -- Build the transaction skeleton
  runBuilderWithConfig cfg sender $ do
    txSkel <- addUtxo crs cfgAccumulationValue (fromJust cfgMaybeScriptRef) ref cfgAddress recipient l r
    unsignedTx <$> buildTxBody txSkel

removeUtxoRun :: UtxoAccumulatorCRS -> Config -> Bool -> IO ()
removeUtxoRun crs cfg@Config {..} removeNoDate = do
  now <- getPOSIXTime
  cache <- fullSyncFromConfig cfg

  -- Get the UTXO accumulator data
  m <-
    filter
      ( \(AccumulatorDataItem _ _ ttRef) ->
          ttRef `elem` keys cache
      )
      <$> getUtxoAccumulatorData cfgDatabasePath
  unless (null m) $ do
    -- Find UTxOs eligible for removal
    let eligible =
          [ (recipient, l, r, ttRef)
          | (AccumulatorDataKey recipient l, AccumulatorDataItem r mDistTime ttRef) <- toList m
          , maybe removeNoDate (<= now) mDistTime
          , utxoAccumulatorHashWrapper (addressToPlutus recipient) l r `elem` fst (cache ! ttRef)
          , utxoAccumulatorAddressHash (addressToPlutus recipient) l `notElem` snd (cache ! ttRef)
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
