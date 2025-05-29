{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.UtxoAccumulator.Precompute where

import Data.Aeson (decode)
import Data.ByteString (fromStrict)
import Data.FileEmbed (embedFileRelative)
import Data.Maybe (fromJust)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)

accumulationGroupElements :: [BLS12_381_G1_Point]
accumulationGroupElements = fromJust $ decode $ fromStrict $(embedFileRelative "accumulation-group-elements.json")

switchGroupElement :: BLS12_381_G1_Point
switchGroupElement = fromJust $ decode $ fromStrict $(embedFileRelative "switch-group-element.json")

distributionGroupElements :: [BLS12_381_G1_Point]
distributionGroupElements = fromJust $ decode $ fromStrict $(embedFileRelative "distribution-group-elements.json")
