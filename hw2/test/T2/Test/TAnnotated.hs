{-# LANGUAGE StandaloneDeriving #-}

module Test.TAnnotated
  ( propAnnotated
  ) where

import HW2.T1 (Annotated (..), mapAnnotated)
import HW2.T2 (distAnnotated, wrapAnnotated)
import Hedgehog (Gen)
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

deriving instance ((Show a, Show e)) => Show (Annotated e a)
deriving instance ((Eq a, Eq e)) => Eq (Annotated e a)

genAnnotated :: Gen (Annotated String String)
genAnnotated = (:#) <$> genString <*> genString

propAnnotated :: TestTree
propAnnotated = allProps "Annotated" genString genAnnotated mapAnnotated wrapAnnotated distAnnotated
