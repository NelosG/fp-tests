module HW1.T3
  ( Tree (..)
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where

import GHC.Generics

-- The Meta field must store additional information about the subtree that can be accessed in constant time, for example its size. You can use Int, (Int, Int), or a custom data structure:
-- 
-- type Meta = Int         -- OK
-- data Meta = M Int Int   -- OK

data Tree a
  = Leaf
  | Branch
    Meta 
    (Tree a)
    a
    (Tree a)


-- | Size of the tree, O(1).
tsize :: Tree a -> Int

-- | Depth of the tree.
tdepth :: Tree a -> Int

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a

-- Tip 1: in order to maintain the CachedSize invariant, define a helper function:
-- 
-- mkBranch :: Tree a -> a -> Tree a -> Tree a
-- Tip 2: the Balanced invariant is the hardest to maintain, so implement it last. Search for “tree rotation”.
