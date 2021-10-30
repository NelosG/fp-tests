module T2SpecAdvanced where

import Hedgehog
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import GHC.Natural (Natural)

import HW1.T2 (N (..), nFromNatural, nToNum, ncmp, nmult, nplus, nsub, nEven, nOdd, ndiv, nmod)

import T2Spec

prop_nEven :: Property
prop_nEven = property $ do
    n <- forAll genNatural
    let nn = nFromNatural' n
    nEven nn === even n

propertyNEven :: TestTree
propertyNEven = testProperty "nEven property" prop_nEven

prop_nOdd :: Property
prop_nOdd = property $ do
    n <- forAll genNatural
    let nn = nFromNatural' n
    nOdd nn === odd n

propertyNOdd :: TestTree
propertyNOdd = testProperty "nOdd property" prop_nOdd

prop_ndiv :: Property
prop_ndiv = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    if b == 0 then discard
    else do
        let an = nFromNatural' a
            bn = nFromNatural' b
        nToNum' (ndiv an bn) === a `div` b

propertyNDiv :: TestTree
propertyNDiv = testProperty "nDiv property" prop_ndiv

prop_nmod :: Property
prop_nmod = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    if b == 0 then discard
    else do
        let an = nFromNatural' a
            bn = nFromNatural' b
        nToNum' (nmod an bn) === a `mod` b

propertyNMod :: TestTree
propertyNMod = testProperty "nMod property" prop_nmod

advancedTests :: TestTree
advancedTests = testGroup "Advanced"
    [
        propertyNEven, propertyNOdd, propertyNDiv, propertyNMod
    ]
