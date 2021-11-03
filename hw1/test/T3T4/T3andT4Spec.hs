module T3andT4Spec
  ( advancedTests
  , tests
  ) where

import qualified Data.Set as Set
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import HW1.T3 (Tree (..), tFromList, tdepth, tinsert, tmember, tsize)
import HW1.T4 (tfoldr)

sorted :: Ord a => [a] -> Bool
sorted []  = True
sorted [x] = True
sorted (x:xs@(y:_))
  | x <= y    = sorted xs
  | otherwise = False

unique :: Ord a => [a] -> Bool
unique xs = helper Set.empty xs
  where helper s [] = True
        helper s (x:xs)
          | Set.member x s = False
          | otherwise      = helper (Set.insert x s) xs

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 10000) Gen.enumBounded

prop_size :: Property
prop_size = property $ do
  xs <- forAll genList
  let tree = tFromList xs
      set  = Set.fromList xs
  tsize tree === Set.size set

propertySize :: TestTree
propertySize = testProperty "tree size" prop_size

prop_sorted :: Property
prop_sorted = property $ do
  xs <- forAll genList
  assert $ sorted ((treeToList . tFromList) xs)

propertySorted :: TestTree
propertySorted = testProperty "tree sorted" prop_sorted

prop_unique :: Property
prop_unique = property $ do
  xs <- forAll genList
  assert $ unique ((treeToList . tFromList) xs)

propertyUnique :: TestTree
propertyUnique = testProperty "tree unique" prop_unique

prop_tFromList :: Property
prop_tFromList = property $ do
  xs <- forAll genList
  (treeToList . tFromList) xs === Set.toAscList (Set.fromList xs)

propertyTFromList :: TestTree
propertyTFromList = testProperty "tree tFromList" prop_tFromList


tests :: TestTree
tests = testGroup "Basic" [propertySize, propertySorted, propertyUnique, propertyTFromList]

-- | Advanced --
tlsize :: Tree a -> Int
tlsize Leaf             = 0
tlsize (Branch _ l _ _) = tsize l

trsize :: Tree a -> Int
trsize Leaf             = 0
trsize (Branch _ _ _ r) = tsize r


isBalancedDepth :: Tree a -> Bool
isBalancedDepth Leaf = True
isBalancedDepth (Branch _ l _ r)
  | abs (tdepth l - tdepth r) <= 1 = isBalancedDepth l && isBalancedDepth r
  | otherwise                      = False

isBalancedSize :: Tree a -> Bool
isBalancedSize Leaf = True
isBalancedSize (Branch _ l _ r)
  | tsize l >= tlsize r && tsize l >= trsize r &&
    tsize r >= tlsize l && tsize r >= trsize l
              = isBalancedSize l && isBalancedSize r
  | otherwise = False

prop_balanced :: Property
prop_balanced = property $ do
  xs <- forAll genList
  let tree = tFromList xs
  assert $ isBalancedDepth tree || isBalancedSize tree

propertyBalanced :: TestTree
propertyBalanced = testProperty "tree balanced" prop_balanced


advancedTests :: TestTree
advancedTests = testGroup "Advanced" [propertyBalanced]
