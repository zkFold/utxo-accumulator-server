module ZkFold.Cardano.UtxoAccumulator.Datum where

import GeniusYield.Types (GYDatum, datumFromPlutusData, datumToPlutus)
import PlutusLedgerApi.V3 (BuiltinByteString, Datum (..), toBuiltinData, unsafeFromBuiltinData)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)
import Prelude (Integer)

updateDatum :: GYDatum -> Integer -> BuiltinByteString -> GYDatum
updateDatum dat n g =
  let Datum d = datumToPlutus dat
      setup = unsafeFromBuiltinData d :: SetupBytes
      setup' = updateSetupBytes setup n g
      d' = toBuiltinData setup'
   in datumFromPlutusData d'
