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
import ZkFold.Cardano.UtxoAccumulator.Constants (protocolStakingCredential)
import ZkFold.Cardano.UtxoAccumulator.Precompute qualified as Precompute
import ZkFold.Data.Vector (init, last)

utxoAccumulatorAddress :: UtxoAccumulatorParameters -> Address
utxoAccumulatorAddress par =
  let sh = scriptHashOf $ utxoAccumulatorCompiled par
   in Address {addressCredential = ScriptCredential sh, addressStakingCredential = Just protocolStakingCredential}

utxoAccumulatorParametersFromAddress :: Value -> Address -> Maybe UtxoAccumulatorParameters
utxoAccumulatorParametersFromAddress v addr =
  find (\par -> utxoAccumulatorAddress par == addr)
    $ distributionParameters v
    ++ accumulationParameters v

distributionParameters :: Value -> [UtxoAccumulatorParameters]
distributionParameters v =
  let lastDistPar :: UtxoAccumulatorParameters
      lastDistPar =
        let maybeSwitchAddress = Nothing
            maybeNextAddress = Nothing
            currentGroupElement = convertG1 $ last Precompute.distributionGroupElements
            switchGroupElement = convertG1 Precompute.switchGroupElement
            accumulationValue = v
         in UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par {maybeNextAddress = Just $ utxoAccumulatorAddress par, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastDistPar]
        (map convertG1 $ toList $ init $ Precompute.distributionGroupElements)

accumulationParameters :: Value -> [UtxoAccumulatorParameters]
accumulationParameters v =
  let lastAccPar :: UtxoAccumulatorParameters
      lastAccPar =
        let maybeSwitchAddress = Just $ utxoAccumulatorAddress $ head $ distributionParameters v
            maybeNextAddress = Nothing
            currentGroupElement = convertG1 $ last Precompute.accumulationGroupElements
            switchGroupElement = convertG1 Precompute.switchGroupElement
            accumulationValue = v
         in UtxoAccumulatorParameters {..}

      prec :: UtxoAccumulatorParameters -> BuiltinByteString -> UtxoAccumulatorParameters
      prec par g = par {maybeNextAddress = Just $ utxoAccumulatorAddress par, currentGroupElement = g}
   in foldr'
        (\g acc -> prec (head acc) g : acc)
        [lastAccPar]
        (map convertG1 $ toList $ init Precompute.accumulationGroupElements)
