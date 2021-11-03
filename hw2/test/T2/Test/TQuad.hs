{-# LANGUAGE StandaloneDeriving #-}

module Test.TQuad
  ( propQuad
  ) where

import HW2.T1 (Quad (..), mapQuad)
import HW2.T2 (distQuad, wrapQuad)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

deriving instance (Show a) => Show (Quad a)
deriving instance (Eq a) => Eq (Quad a)

propQuad :: TestTree
propQuad = allProps "Quad" genString genQuad mapQuad wrapQuad distQuad

genQuad :: Gen (Quad String)
genQuad = Q <$> genString <*> genString <*> genString <*> genString
