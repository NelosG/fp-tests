module HW1.T3
       ( Tree (..),
       tsize,
       tdepth,
       tmember,
       tinsert,
       tFromList
       ) where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int

-- | Depth of the tree.
tdepth :: Tree a -> Int


-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool


-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a



-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a

-- support functions

mkBranch :: Tree a -> a -> Tree a -> Tree a
