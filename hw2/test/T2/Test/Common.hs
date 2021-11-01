{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Test.Common
  where

import Hedgehog (Gen, Property, diff, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genString :: Gen String
genString = Gen.string (Range.linear 1 100) Gen.alpha

homProp :: (Show (m (String, String)), Eq (m (String, String))) =>
     (forall a.a -> m a)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
homProp wrapF distF = property $ do
    i <- forAll genString
    j <- forAll genString
    distF (wrapF i, wrapF j) === wrapF (i, j) -- No isomorphisms apply

assocProp :: (Show (m String), Show (m ((String, String), String)), Eq (m ((String, String), String))) =>
     Gen (m String)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
assocProp genF mapF distF = property $ do
    p <- forAll genF
    q <- forAll genF
    r <- forAll genF
    let assocIso = mapF $ \(a, (b, c)) -> ((a, b), c)
    assocIso (distF (p, distF (q, r))) === distF (distF (p, q), r)

idProp :: (Show (m String), Eq (m String)) =>
     Gen (m String)
  -> (forall a.a -> m a)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> Property
idProp genF wrapF mapF distF = property $ do
    p <- forAll genF
    q <- forAll genF
    let leftIdIso = mapF $ \((), a) -> a
    leftIdIso (distF (wrapF (), q)) === q

-- this is fine.
allProps :: (forall t.(Show t) => (Show (m t)), forall t.(Eq t) => (Eq (m t))) =>
     String
  -> Gen (m String)
  -> (forall a b.(a -> b) -> m a -> m b)
  -> (forall a.a -> m a)
  -> (forall a b.(m a, m b) -> m (a, b))
  -> TestTree
allProps name genF mapF wrapF distF = testGroup (name ++ " properties") [
    testProperty (name ++ " homomorphism") $ homProp wrapF distF
  , testProperty (name ++ " associativity") $ assocProp genF mapF distF
  , testProperty (name ++ " associativity") $ idProp genF wrapF mapF distF
  ]
