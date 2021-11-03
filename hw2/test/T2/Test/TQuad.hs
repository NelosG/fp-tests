{-# LANGUAGE StandaloneDeriving #-}

module Test.TQuad
  where

import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Tasty

deriving instance (Show a) => Show (Quad a)
deriving instance (Eq a) => Eq (Quad a)

propQuad :: TestTree
propQuad = allProps "Quad" genString genQuad mapQuad wrapQuad distQuad

genQuad :: Gen (Quad String)
genQuad = Q <$> genString <*> genString <*> genString <*> genString
