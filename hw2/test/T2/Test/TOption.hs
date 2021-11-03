{-# LANGUAGE StandaloneDeriving #-}

module Test.TOption
  where

import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Tasty

deriving instance (Show a) => Show (Option a)
deriving instance (Eq a) => Eq (Option a)

propOption :: TestTree
propOption = allProps "Option" genString genOption mapOption wrapOption distOption

genOption :: Gen (Option String)
genOption = Gen.choice [genNone, genSome]
  where
    genNone = Gen.constant None
    genSome = Some <$> genString
