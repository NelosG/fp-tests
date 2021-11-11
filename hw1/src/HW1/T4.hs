module HW1.T4
  ( tfoldr
  ) where

import HW1.T3 (Tree (Branch, Leaf))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
