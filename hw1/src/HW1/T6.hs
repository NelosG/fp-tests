module HW1.T6
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
