{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.UtxoAccumulator.Precompute where

import ZkFold.Cardano.UtxoAccumulator.Constants (N)
import ZkFold.Data.Vector (Vector, unsafeToVector)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import Data.FileEmbed (embedFileRelative)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Data.ByteString (fromStrict)
-- import ZkFold.Symbolic.Examples.UtxoAccumulator (
--   accumulationGroupElements,
--   distributionGroupElements,
--  )
-- import qualified ZkFold.Symbolic.Examples.UtxoAccumulator as S

accumulationGroupElements :: Vector N BLS12_381_G1_Point
accumulationGroupElements = unsafeToVector $ fromJust $ decode $ fromStrict $(embedFileRelative "accumulation-group-elements.json")

switchGroupElement :: BLS12_381_G1_Point
switchGroupElement = fromJust $ decode $ fromStrict $(embedFileRelative "switch-group-element.json")

distributionGroupElements :: Vector N BLS12_381_G1_Point
distributionGroupElements = unsafeToVector $ fromJust $ decode $ fromStrict $(embedFileRelative "distribution-group-elements.json")
