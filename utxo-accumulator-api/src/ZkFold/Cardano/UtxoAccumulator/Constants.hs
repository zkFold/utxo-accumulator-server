module ZkFold.Cardano.UtxoAccumulator.Constants where

import GeniusYield.Types
import PlutusLedgerApi.V3 (Value, StakingCredential (..))
import ZkFold.Cardano.OffChain.Plonkup (mkSetup)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Symbolic.Examples.UtxoAccumulator (utxoAccumulatorVerifierSetup)
import Prelude (($), error)
import GeniusYield.Scripts.TestToken (testTokenPolicy)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

type N = 10
type M = 1024

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

-- TODO: adjust the constant
protocolTreasuryAddress :: GYAddress
protocolTreasuryAddress = fromJust $
    addressFromTextMaybe "addr_test1qzz6f0p5u0p6fkm8yrtphaczrtlj49zlexh654nrgvp3atkcvu6hdrunpqkfygtup4vmz90acsaemlf8t5r7lua2wtlstf4kv9"

-- TODO: adjust the constant
protocolStakingCredential :: StakingCredential
protocolStakingCredential =
    StakingHash $ stakeCredentialToPlutus $ GYStakeCredentialByKey "d86735768f93082c92217c0d59b115fdc43b9dfd275d07eff3aa72ff"
