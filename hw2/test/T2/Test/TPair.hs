{-# LANGUAGE StandaloneDeriving #-}

module Test.TPair
  ( propPair
  ) where

import HW2.T1 (Pair (..), mapPair)
import HW2.T2 (distPair, wrapPair)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

deriving instance (Show a) => Show (Pair a)
deriving instance (Eq a) => Eq (Pair a)

propPair :: TestTree
propPair = allProps "Pair" genString genPair mapPair wrapPair distPair

genPair :: Gen (Pair String)
genPair = P <$> genString <*> genString
