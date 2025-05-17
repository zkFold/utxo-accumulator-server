module ZkFold.Cardano.UtxoAccumulator.Constants where

import GeniusYield.Types (GYAssetClass (..), GYTokenName (..), GYScript, PlutusVersion (..), GYTxOutRef, mintingPolicyId, GYAddress, GYValue, valueFromPlutus)
import GeniusYield.Types.Ada (lovelaceValueOf)
import PlutusLedgerApi.V3 (Value, StakingCredential (..))
import ZkFold.Cardano.OffChain.Plonkup (mkSetup)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Symbolic.Examples.UtxoAccumulator (utxoAccumulatorVerifierSetup)
import Prelude (($), undefined, error)
import GeniusYield.Scripts.TestToken (testTokenPolicy)
import Data.Either (fromRight)

type N = 100
type M = 4096 -- 2^12

threadTokenName :: GYTokenName
threadTokenName = GYTokenName ""

threadTokenPolicy :: GYTxOutRef -> GYScript 'PlutusV2
threadTokenPolicy = testTokenPolicy 1 threadTokenName

threadToken :: GYTxOutRef -> GYAssetClass
threadToken ref = GYToken (mintingPolicyId $ threadTokenPolicy ref) threadTokenName

utxoAccumulatorSetupBytesInit :: SetupBytes
utxoAccumulatorSetupBytesInit = mkSetup $ utxoAccumulatorVerifierSetup @N @M

serverDeposit :: GYValue
serverDeposit = fromRight (error "Parsing value failed.") $ valueFromPlutus $ lovelaceValueOf 2_000_000

serverFee :: Value
serverFee = lovelaceValueOf 5_000_000

protocolFee :: Value
protocolFee = lovelaceValueOf 1_000_000

protocolTreasuryAddress :: GYAddress
protocolTreasuryAddress = undefined

protocolStakingCredential :: StakingCredential
protocolStakingCredential = undefined
