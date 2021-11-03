{-# LANGUAGE StandaloneDeriving #-}

module Test.TPrioritised
  where

import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Tasty

deriving instance (Show a) => Show (Prioritised a)
deriving instance (Eq a) => Eq (Prioritised a)

propPrioritised :: TestTree
propPrioritised = allProps "Prioritised" genString genPrioritised mapPrioritised wrapPrioritised distPrioritised

genPrioritised :: Gen (Prioritised String)
genPrioritised = Gen.choice [genLow, genMedium, genHigh]
  where
    genLow = Low <$> genString
    genMedium = Medium <$> genString
    genHigh = High <$> genString
