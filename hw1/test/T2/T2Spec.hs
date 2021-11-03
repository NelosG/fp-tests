module T2Spec
  ( genNatural
  , nFromNatural'
  , nToNum'
  , tests
  ) where

import GHC.Natural (Natural)
import HW1.T2 (N (..), nFromNatural, nToNum, ncmp, nmult, nplus, nsub)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec ()

nFromNatural' :: Natural -> N
nFromNatural' 0 = Z
nFromNatural' n = S $ nFromNatural' (n - 1)

nToNum' :: Num a => N -> a
nToNum' Z     = 0
nToNum' (S n) = 1 + nToNum' n

genNatural :: Gen Natural
genNatural = Gen.integral_ $ Range.linear 0 100

prop_nplus :: Property
prop_nplus = property $ do
  a <- forAll genNatural
  b <- forAll genNatural
  let an = nFromNatural' a
      bn = nFromNatural' b
  nToNum' (nplus an bn) === a + b

propertyNplus :: TestTree
propertyNplus = testProperty "nplus property" prop_nplus

prop_nmult :: Property
prop_nmult = property $ do
  a <- forAll genNatural
  b <- forAll genNatural
  let an = nFromNatural' a
      bn = nFromNatural' b
  nToNum' (nmult an bn) === a * b

propertyNmult :: TestTree
propertyNmult = testProperty "nmult property" prop_nmult

prop_nsub :: Property
prop_nsub = property $ do
  a <- forAll genNatural
  b <- forAll genNatural
  let an = nFromNatural' a
      bn = nFromNatural' b
  case nsub an bn of
    Just cn -> nToNum' cn === a - b
    Nothing -> assert $ a < b

propertyNsub :: TestTree
propertyNsub = testProperty "nsub property" prop_nsub

prop_ncmp :: Property
prop_ncmp = property $ do
  a <- forAll genNatural
  b <- forAll genNatural
  let an = nFromNatural' a
      bn = nFromNatural' b
  ncmp an bn === compare a b

propertyNcmp :: TestTree
propertyNcmp = testProperty "ncmp property" prop_ncmp

prop_nFromNatural :: Property
prop_nFromNatural = property $ do
  n <- forAll genNatural
  nToNum' (nFromNatural n) === n

propertyNFromNatural :: TestTree
propertyNFromNatural = testProperty "nFromNatural property" prop_nFromNatural

prop_nToNum :: Property
prop_nToNum = property $ do
  n <- forAll genNatural
  let nn = nFromNatural' n
  nToNum nn === nToNum' nn

propertyNToNum :: TestTree
propertyNToNum = testProperty "nToNum property" prop_nToNum

tests :: TestTree
tests = testGroup "Basic" [
    propertyNplus, propertyNmult, propertyNsub
    , propertyNcmp, propertyNFromNatural, propertyNToNum
  ]
