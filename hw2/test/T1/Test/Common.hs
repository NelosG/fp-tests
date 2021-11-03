{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Common
  ( allProps
  , compProp
  , genFun
  , genInt
  , idProp
  ) where

import Control.Monad (liftM2)
import HW2.T1
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genInt :: Gen Int
genInt = Gen.int $ Range.linear 1 100

instance Show a => Show (Fun Int a) where
  show (F f) = "f([-10..10])=" ++ show (map f [-10..10])

idProp :: ((Show (m Int), Eq (m Int))) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
idProp genF mapF = property $ do
  m <- forAll genF
  mapF id m === m

compProp :: (Show (m Int), Eq (m Int)) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
compProp genF mapF = property $ do
  m <- forAll genF
  (F f) <- forAll genFun
  (F g) <- forAll genFun
  (mapF f . mapF g) m === mapF (f . g) m

genFun :: Gen (Fun Int Int)
genFun = F <$> do
  op <- Gen.choice [Gen.constant (+), Gen.constant (-), Gen.constant (*)]
  op <$> genInt

allProps :: (Show (m Int), Eq (m Int)) => String
  -> Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> TestTree
allProps name genF mapF = testGroup (name ++" properties") [
    testProperty (name ++ " id property") $ idProp genF mapF
  , testProperty (name ++ " composition property") $ compProp genF mapF
  ]
