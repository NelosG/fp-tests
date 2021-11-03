{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

module Test.Common
  ( allProps
  , genString
  ) where

import Data.Kind (Constraint)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genString :: Gen String
genString = Gen.string (Range.linear 1 100) Gen.alpha

assocProp :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t))) =>
     Gen (m String)
  -> (forall a.m (m a) -> m a)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a.a -> m a)
  -> Property
assocProp genF joinF mapF wrapF = property $ do
  m <- forAll genF
  let m' = wrapF $ wrapF m
  joinF (mapF joinF m') === joinF (joinF m')

idProp :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t))) =>
     Gen (m String)
  -> (forall a.m (m a) -> m a)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a.a -> m a)
  -> Property
idProp genF joinF mapF wrapF = property $ do
  m <- forAll genF
  joinF (wrapF m) === m
  joinF (mapF wrapF m) === m

distProp :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t))) =>
     Gen (m String)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> (forall a.m (m a) -> m a)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> Property
distProp genF distF joinF mapF = property $ do
  p <- forAll genF
  q <- forAll genF
  let distF' (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)
  distF' (p, q) === distF (p, q)

allProps :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t))) =>
     String
  -> Gen (m String)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a.a -> m a)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> (forall a.m (m a) -> m a)
  -> TestTree
allProps name genF mapF wrapF distF joinF = testGroup (name ++ " properties") [
    testProperty (name ++ " associativity") $ assocProp genF joinF mapF wrapF
  , testProperty (name ++ " left/right identity") $ idProp genF joinF mapF wrapF
  , testProperty (name ++ " distribution through join") $ distProp genF distF joinF mapF
  ]
