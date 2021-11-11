module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = 
  a :+ ListPlus a
  | Last a       
infixr 5 :+

instance Semigroup (ListPlus a) where
  ...

data Inclusive a b = 
  This a 
  | That b 
  | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  ...

newtype DotString = DS String

instance Semigroup DotString where
  ...

instance Monoid DotString where
  ...

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  ...

instance Monoid (Fun a) where
  ...
