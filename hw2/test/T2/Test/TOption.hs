{-# LANGUAGE StandaloneDeriving #-}

module Test.TOption
  ( propOption
  ) where

import HW2.T1 (Option (..), mapOption)
import HW2.T2 (distOption, wrapOption)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

deriving instance (Show a) => Show (Option a)
deriving instance (Eq a) => Eq (Option a)

propOption :: TestTree
propOption = allProps "Option" genString genOption mapOption wrapOption distOption

genOption :: Gen (Option String)
genOption = Gen.choice [genNone, genSome]
  where
    genNone = Gen.constant None
    genSome = Some <$> genString
