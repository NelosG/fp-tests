module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]

joinWith :: a -> NonEmpty [a] -> [a]
