{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Tree
  ( hspecTree
  , propTree
  ) where
import Data.Foldable (Foldable (foldr'))
import HW2.T1 (Tree (Branch, Leaf), mapTree)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Common (allProps, genInt)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a) => Show (Tree a)
deriving instance (Eq a) => Eq (Tree a)

tInsert :: Ord a => a -> Tree a -> Tree a
tInsert a Leaf = Branch Leaf a Leaf
tInsert a tree@(Branch l v r)
  | a < v = Branch (tInsert a l) v Leaf
  | a > v = Branch l v (tInsert a r)
  | otherwise = tree

--Creates right "bamboo"
tInsertSec :: Ord a => a -> Tree a -> Tree a
tInsertSec a Leaf           = Branch Leaf a Leaf
tInsertSec a (Branch l v r) = tInsertSec a r

tFromList :: Ord a => [a] -> Tree a
tFromList = tFromListIns tInsert

tFromListIns :: Ord a => (a -> Tree a -> Tree a) -> [a] -> Tree a
tFromListIns ins = foldr' ins Leaf

hspecTree :: IO TestTree
hspecTree = testSpec "Tree tests:" $ do
  it "Tree test1" $ mapTree (+ 1) (tFromList [1, 2, 3]) `shouldBe` tFromList [2, 3, 4]
  it "Tree test2" $ mapTree (+ 1) (tFromListIns tInsertSec [1, 2, 3]) `shouldBe` tFromListIns tInsertSec [2, 3, 4]
  it "Tree(f . g) test" $ (mapTree (+ 1) . mapTree (* 10)) (tFromList [1, 2, 3]) `shouldBe` tFromList [11, 21, 31]

genTree :: Gen (Tree Int)
genTree = Gen.frequency [(2, Gen.constant Leaf), (1, genBranch)]
  where
    genBranch = Branch <$> genTree <*> genInt <*> genTree

propTree :: TestTree
propTree = allProps "Tree" genTree mapTree
