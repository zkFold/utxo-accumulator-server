module ZkFold.Cardano.UtxoAccumulator.Constants where

import Data.List (head)
import Data.Maybe (fromJust)
import GeniusYield.Scripts.TestToken (testTokenPolicy)
import GeniusYield.TxBuilder (GYTxQueryMonad, addressFromPlutus')
import GeniusYield.Types
import PlutusLedgerApi.V3 (Address (..), Credential (..), StakingCredential (..), ToData (..))
import ZkFold.Cardano.OffChain.Plonkup (mkSetup)
import ZkFold.Cardano.OffChain.Utils (scriptHashOf)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.UPLC.Common (parkingSpotCompiled)
import ZkFold.Cardano.UPLC.UtxoAccumulator (UtxoAccumulatorParameters, utxoAccumulatorCompiled)
import ZkFold.Cardano.UtxoAccumulator.Datum (addDatums, removeDatums)
import ZkFold.Prelude (readFileJSON)
import ZkFold.Symbolic.Examples.UtxoAccumulator (UtxoAccumulatorCRS (..), utxoAccumulatorVerifierSetup)
import Prelude (IO, Integer, Maybe (..), ($), (.))

-- Thread token

threadTokenName :: GYTokenName
threadTokenName = GYTokenName ""

threadTokenPolicy :: GYTxOutRef -> GYScript 'PlutusV2
threadTokenPolicy = testTokenPolicy 1 threadTokenName

threadToken :: GYTxOutRef -> GYAssetClass
threadToken ref = GYToken (mintingPolicyId $ threadTokenPolicy ref) threadTokenName

-- Script parking

scriptParkingParameter :: Integer
scriptParkingParameter = 42

scriptParkingAddress :: GYTxQueryMonad m => m GYAddress
scriptParkingAddress =
  let sh = scriptHashOf $ parkingSpotCompiled scriptParkingParameter
   in addressFromPlutus' $
        Address
          { addressCredential = ScriptCredential sh
          , addressStakingCredential = Just protocolStakingCredential
          }

-- Protocol parameters

serverFee :: GYValue
serverFee = valueFromLovelace 5_000_000

protocolFee :: GYValue
protocolFee = valueFromLovelace 1_000_000

-- TODO: adjust the constant
protocolTreasuryAddress :: GYAddress
protocolTreasuryAddress =
  fromJust $
    addressFromTextMaybe "addr_test1qzz6f0p5u0p6fkm8yrtphaczrtlj49zlexh654nrgvp3atkcvu6hdrunpqkfygtup4vmz90acsaemlf8t5r7lua2wtlstf4kv9"

-- TODO: adjust the constant
protocolStakingCredential :: StakingCredential
protocolStakingCredential =
  StakingHash $ stakeCredentialToPlutus $ GYStakeCredentialByKey "d86735768f93082c92217c0d59b115fdc43b9dfd275d07eff3aa72ff"

-- Utxo Accumulator

type N = 10
type M = 1024

-- type N = 1024
-- type M = 16384

utxoAccumulatorParameters :: GYValue -> UtxoAccumulatorParameters
utxoAccumulatorParameters = valueToPlutus

utxoAccumulatorScript :: GYValue -> GYScript 'PlutusV3
utxoAccumulatorScript = scriptFromPlutus . utxoAccumulatorCompiled . utxoAccumulatorParameters

utxoAccumulatorBuildScript :: GYTxOutRef -> GYValue -> GYBuildPlutusScript 'PlutusV3
utxoAccumulatorBuildScript ref = GYInReference @_ @PlutusV3 ref . utxoAccumulatorScript

utxoAccumulatorAddress :: GYTxQueryMonad m => GYValue -> m GYAddress
utxoAccumulatorAddress v =
  let sh = scriptHashOf $ utxoAccumulatorCompiled $ utxoAccumulatorParameters v
   in addressFromPlutus' $
        Address
          { addressCredential = ScriptCredential sh
          , addressStakingCredential = Just protocolStakingCredential
          }

utxoAccumulatorCRS :: IO UtxoAccumulatorCRS
utxoAccumulatorCRS = readFileJSON "utxo-accumulator-api/data/crs.json"

utxoAccumulatorSetupBytesInit :: UtxoAccumulatorCRS -> SetupBytes
utxoAccumulatorSetupBytesInit = mkSetup . utxoAccumulatorVerifierSetup @N @M

utxoAccumulatorDatumInit :: UtxoAccumulatorCRS -> GYDatum
utxoAccumulatorDatumInit crs =
  let addDatum = head $ addDatums crs
      removeDatum = head $ removeDatums crs
      setup = utxoAccumulatorSetupBytesInit crs
   in datumFromPlutusData $ toBuiltinData (addDatum, removeDatum, setup)
