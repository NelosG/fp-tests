module Test.Common
  ( allProps
  , compProp
  , genInt
  , idProp
  ) where

import Control.Monad (liftM2)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genInt :: Gen Int
genInt = Gen.int $ Range.linear 1 100

idProp :: ((Show (m Int), Eq (m Int))) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
idProp genF mapF = property $ do
    m <- forAll genF
    mapF id m === m

compProp :: ((Show (m Int), Eq (m Int))) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
compProp genF mapF = property $ do
  m <- forAll genF
  let check f g = (mapF f . mapF g) m === mapF (f . g) m
  check (+2) (+2)
  check (+2) (*3)
  check (*3) (+2)
  check (*3) (*3)

allProps :: ((Show (m Int), Eq (m Int))) => String
  -> Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> TestTree
allProps name genF mapF = testGroup (name ++" properties") [
    testProperty (name ++ " id property") $ idProp genF mapF
  , testProperty (name ++ " composition property") $ compProp genF mapF
  ]
