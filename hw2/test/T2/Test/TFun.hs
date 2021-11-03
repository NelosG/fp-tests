{-# LANGUAGE FlexibleInstances #-}
module Test.TFun
  where

import HW2.T1 (Fun (F), mapFun)
import HW2.T2
import HW2.T3
import Hedgehog hiding (test)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog hiding (test)
import Test.Tasty.Hspec
import Text.Show (Show)
import qualified Data.Bifunctor
import Test.TFun.Ident (testIdentRight, testIdentLeft)
import Test.TFun.Homo ( testHomo )
import Test.TFun.Assoc (testAssoc)


hspecFun :: IO TestTree
hspecFun = testSpec "Fun tests:" $ do
    it "Homomorphism" testHomo
    it "Associativity" testAssoc
    it "Right identity" testIdentRight
    it "Left identity" testIdentLeft

