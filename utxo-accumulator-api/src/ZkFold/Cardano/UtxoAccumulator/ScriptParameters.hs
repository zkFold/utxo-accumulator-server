module ZkFold.Cardano.UtxoAccumulator.ScriptParameters where

import Data.Foldable (foldr', toList)
import PlutusLedgerApi.V3 (Address (..), Credential (..), Value)
import PlutusTx.Prelude (
  BuiltinByteString,
  Maybe (..),
  head,
  ($),
 )
import Prelude (map, (==))

import Data.List (find, (++))
import ZkFold.Cardano.OffChain.BLS12_381 (convertG1)
import ZkFold.Cardano.OffChain.Utils (scriptHashOf)
import ZkFold.Cardano.UPLC.UtxoAccumulator (
  UtxoAccumulatorParameters (..),
  utxoAccumulatorCompiled,
 )
import ZkFold.Cardano.UtxoAccumulator.Constants (M, N, protocolStakingCredential)
import ZkFold.Data.Vector (init, last)
import ZkFold.Symbolic.Examples.UtxoAccumulator (
  accumulationGroupElements,
  distributionGroupElements,
 )
import qualified ZkFold.Symbolic.Examples.UtxoAccumulator as S

utxoAccumulatorAddress :: UtxoAccumulatorParameters -> Address
utxoAccumulatorAddress par =
  let sh = scriptHashOf $ utxoAccumulatorCompiled par
   in Address {addressCredential = ScriptCredential sh, addressStakingCredential = Just protocolStakingCredential}

utxoAccumulatorParametersFromAddress :: Value -> Address -> Maybe UtxoAccumulatorParameters
utxoAccumulatorParametersFromAddress v addr =
  find (\par -> utxoAccumulatorAddress par == addr) $
    distributionParameters v ++ accumulationParameters v

distributionParameters :: Value -> [UtxoAccumulatorParameters]
distributionParameters v =
  let lastDistPar :: UtxoAccumulatorParameters
      lastDistPar =
        let maybeSwitchAddress = Nothing
            maybeNextAddress = Nothing
            currentGroupElement = convertG1 $ last $ distributionGroupElements @N @M
            switchGroupElement = convertG1 $ S.switchGroupElement @N @M
            accumulationValue = v
         in UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par {maybeNextAddress = Just $ utxoAccumulatorAddress par, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastDistPar]
        (map convertG1 $ toList $ init $ distributionGroupElements @N @M)

accumulationParameters :: Value -> [UtxoAccumulatorParameters]
accumulationParameters v =
  let lastAccPar :: UtxoAccumulatorParameters
      lastAccPar =
        let maybeSwitchAddress = Just $ utxoAccumulatorAddress $ head $ distributionParameters v
            maybeNextAddress = Nothing
            currentGroupElement = convertG1 $ last $ accumulationGroupElements @N @M
            switchGroupElement = convertG1 $ S.switchGroupElement @N @M
            accumulationValue = v
         in UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par {maybeNextAddress = Just $ utxoAccumulatorAddress par, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastAccPar]
        (map convertG1 $ toList $ init $ accumulationGroupElements @N @M)
