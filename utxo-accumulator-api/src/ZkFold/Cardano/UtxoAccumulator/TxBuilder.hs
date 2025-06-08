module ZkFold.Cardano.UtxoAccumulator.TxBuilder where

import Control.Monad (unless)
import Data.Map (delete, insert, toList)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusTx.Builtins (ByteOrder (..), blake2b_224, byteStringToInteger, serialiseData)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.UtxoAccumulator.Constants (threadToken)
import ZkFold.Cardano.UtxoAccumulator.Database (AccumulatorDataItem (..), AccumulatorDataKey (AccumulatorDataKey), UtxoDistributionTime, getUtxoAccumulatorData, putUtxoAccumulatorData, removeUtxoAccumulatorData)
import ZkFold.Cardano.UtxoAccumulator.Sync (fullSync)
import ZkFold.Cardano.UtxoAccumulator.Transition (utxoAccumulatorHashWrapper)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Internal (addUtxo, initAccumulator, postScript, removeUtxo)
import ZkFold.Cardano.UtxoAccumulator.TxBuilder.Utils (getState)
import ZkFold.Cardano.UtxoAccumulator.Types.Config (Config (..))
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS)

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
  UtxoAccumulatorCRS ->
  Config ->
  IO Config
initAccumulatorRun crs cfg@Config {..} = do
  removeUtxoAccumulatorData cfgDatabasePath
  runSignerWithConfig cfg $ do
    (txSkel, ref) <- initAccumulator crs cfgAddress cfgAccumulationValue
    txBody <- buildTxBody txSkel
    submitTxBodyConfirmed_ txBody [cfgPaymentKey]
    return $ cfg {cfgMaybeThreadTokenRef = Just ref}

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
  let key = AccumulatorDataKey recipient l
      item = AccumulatorDataItem r distTime (fromJust cfgMaybeThreadTokenRef)
  putUtxoAccumulatorData cfgDatabasePath $ insert key item m

  -- Build the transaction skeleton
  runBuilderWithConfig cfg sender $ do
    txSkel <- addUtxo crs cfgAccumulationValue (fromJust cfgMaybeScriptRef) (fromJust cfgMaybeThreadTokenRef) cfgAddress recipient l r
    unsignedTx <$> buildTxBody txSkel

removeUtxoRun :: UtxoAccumulatorCRS -> Config -> Bool -> IO ()
removeUtxoRun crs cfg@Config {..} removeNoDate = do
  -- Get the UTXO accumulator data
  m <- getUtxoAccumulatorData cfgDatabasePath
  unless (null m) $ do
    stateRef <- runQueryWithConfig cfg $ fromJust <$> getState (threadToken $ fromJust cfgMaybeThreadTokenRef)
    (hs, as) <- fullSync cfg stateRef
    now <- getPOSIXTime

    -- Find UTxOs eligible for removal
    let eligible =
          [ (recipient, l, r)
          | (AccumulatorDataKey recipient l, AccumulatorDataItem r mDistTime ttRef) <- toList m
          , ttRef == fromJust cfgMaybeThreadTokenRef
          , maybe removeNoDate (<= now) mDistTime
          , toZp (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData $ addressToPlutus recipient) `notElem` as
          ]
    case eligible of
      [] -> return () -- No eligible UTxOs to remove at this time
      ((recipient, l, r) : _) -> do
        let (hs', as') = case cfgMaestroToken of
              "" -> ([utxoAccumulatorHashWrapper (addressToPlutus recipient) l r], [])
              _ -> (hs, as)

        -- Build, sign, and submit the transaction
        runSignerWithConfig cfg $ do
          txSkel <- removeUtxo crs cfgAccumulationValue (fromJust cfgMaybeScriptRef) (fromJust cfgMaybeThreadTokenRef) hs' as' recipient l r
          txBody <- buildTxBody txSkel
          submitTxBodyConfirmed_ txBody [cfgPaymentKey]

        -- Update the database by removing the UTXO
        putUtxoAccumulatorData cfgDatabasePath $ delete (AccumulatorDataKey recipient l) m
        -- Repeat until all eligible UTxOs are removed
        removeUtxoRun crs cfg removeNoDate
