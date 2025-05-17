module ZkFold.Cardano.UtxoAccumulator.Datum where

import PlutusLedgerApi.V3 (toBuiltinData, BuiltinByteString, Datum (..), unsafeFromBuiltinData)
import Prelude (Integer)
import GeniusYield.Types (GYDatum, datumToPlutus, datumFromPlutusData)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)

updateDatum :: GYDatum -> Integer -> BuiltinByteString -> GYDatum
updateDatum dat n g =
  let Datum d = datumToPlutus dat
      setup = unsafeFromBuiltinData d :: SetupBytes
      setup' = updateSetupBytes setup n g
      d' = toBuiltinData setup'
   in datumFromPlutusData d'