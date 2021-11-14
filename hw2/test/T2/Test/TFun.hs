{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Test.TFun
  ( hspecFun
  , propFun
  ) where

import qualified Data.Bifunctor
import HW2.T1 (Fun (F), mapFun)
import HW2.T2 (distFun, wrapFun)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps)
import Test.Hspec (it)
import Test.TFun.Assoc (testAssoc)
import Test.TFun.Homo (testHomo)
import Test.TFun.Ident (testIdentLeft, testIdentRight)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog ()
import Test.Tasty.Hspec (testSpec)
import Text.Show (Show)

hspecFun :: IO TestTree
hspecFun = testSpec "Fun tests:" $ do
  it "Homomorphism" testHomo
  it "Associativity" testAssoc
  it "Right identity" testIdentRight
  it "Left identity" testIdentLeft

instance Eq a => Eq (Fun Int a) where
  (==) (F f1) (F f2) = map f1 [-10..10] == map f2 [-10..10]

instance Show a => Show (Fun Int a) where
  show (F f) = "f([-10..10])=" ++ show (map f [-10..10])

genInt :: Gen Int
genInt = Gen.int $ Range.linear 1 100

genFun :: Gen (Fun Int Int)
genFun = F <$> do
  op <- Gen.choice [Gen.constant (+), Gen.constant (-), Gen.constant (*)]
  op <$> genInt

propFun :: TestTree
propFun = allProps "Fun" genInt genFun mapFun wrapFun distFun
