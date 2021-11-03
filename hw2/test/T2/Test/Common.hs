{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

module Test.Common
  ( allProps
  , genString
  ) where

import Hedgehog (Gen, Property, diff, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genString :: Gen String
genString = Gen.string (Range.linear 1 100) Gen.alpha

homProp :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t)), Eq t, Show t) =>
     Gen t
  -> (forall a.a -> m a)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
homProp genS wrapF distF = property $ do
  i <- forAll genS
  j <- forAll genS
  distF (wrapF i, wrapF j) === wrapF (i, j) -- No isomorphisms apply

assocProp :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t)), Eq t, Show t) =>
     Gen (m t)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
assocProp genF mapF distF = property $ do
  p <- forAll genF
  q <- forAll genF
  r <- forAll genF
  let assocIso = mapF $ \(a, (b, c)) -> ((a, b), c)
  assocIso (distF (p, distF (q, r))) === distF (distF (p, q), r)

idProp :: (Show (m t), Eq (m t)) =>
     Gen (m t)
  -> (forall a.a -> m a)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
idProp genF wrapF mapF distF = property $ do
  p <- forAll genF
  q <- forAll genF
  let leftIdIso = mapF $ \((), a) -> a
  let rightIdIso = mapF $ \(a, ()) -> a
  leftIdIso (distF (wrapF (), q)) === q
  rightIdIso (distF (p, wrapF ())) === p

-- this is fine.
allProps :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t)), Eq t, Show t) =>
     String
  -> Gen t
  -> Gen (m t)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a.a -> m a)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> TestTree
allProps name genS genF mapF wrapF distF = testGroup (name ++ " properties") [
    testProperty (name ++ " homomorphism") $ homProp genS wrapF distF
  , testProperty (name ++ " associativity") $ assocProp genF mapF distF
  , testProperty (name ++ " left/right identity") $ idProp genF wrapF mapF distF
  ]
