{-# LANGUAGE StandaloneDeriving #-}

module Test.TAnnotated
  where

import HW2.T1
import HW2.T2
import Hedgehog (Gen)
import Test.Common
import Test.Tasty
import Test.Tasty.Hedgehog

deriving instance ((Show a, Show e)) => Show (Annotated e a)
deriving instance ((Eq a, Eq e)) => Eq (Annotated e a)

genAnnotated :: Gen (Annotated String String)
genAnnotated = (:#) <$> genString <*> genString

propAnnotated :: TestTree
propAnnotated = allProps "Annotated" genAnnotated mapAnnotated wrapAnnotated distAnnotated
