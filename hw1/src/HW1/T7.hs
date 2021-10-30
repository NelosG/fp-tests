module HW1.T7
       ( ListPlus(..),
       Inclusive(..),
       DotString(..),
       Fun(..)
       ) where
-- Define the following data type and a lawful Semigroup instance for it

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where


data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where



newtype DotString = DS String deriving Show

instance Semigroup DotString where

-- ghci> DS "person" <> DS "address" <> DS "city"
-- DS "person.address.city"

-- Implement a Monoid instance for it

-- mempty <> a  ≡  a
-- a <> mempty  ≡  a

instance Monoid DotString where


-- Implement lawful Semigroup and Monoid instances for it
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where

instance Monoid (Fun a) where
