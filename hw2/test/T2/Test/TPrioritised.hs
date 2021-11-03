{-# LANGUAGE StandaloneDeriving #-}

module Test.TPrioritised
  ( propPrioritised
  ) where

import HW2.T1 (Prioritised (..), mapPrioritised)
import HW2.T2 (distPrioritised, wrapPrioritised)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

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
