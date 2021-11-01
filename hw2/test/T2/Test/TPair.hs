{-# LANGUAGE StandaloneDeriving #-}

module Test.TPair
  where

import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Tasty

deriving instance (Show a) => Show (Pair a)
deriving instance (Eq a) => Eq (Pair a)

propPair :: TestTree
propPair = allProps "Pair" genPair mapPair wrapPair distPair

genPair :: Gen (Pair String)
genPair = P <$> genString <*> genString
