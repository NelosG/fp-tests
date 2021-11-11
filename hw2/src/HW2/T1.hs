module HW2.T1
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude ()

data Option a
  = None
  | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a
  = Error e
  | Success a

data Prioritised a
  = Low a
  | Medium a
  | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a
  = Nil
  | a :. List a
infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a
  = Leaf
  | Branch
    (Tree a)
    a
    (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)

mapPair :: (a -> b) -> (Pair a -> Pair b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)

mapExcept :: (a -> b) -> (Except e a -> Except e b)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)

mapStream :: (a -> b) -> (Stream a -> Stream b)

mapList :: (a -> b) -> (List a -> List b)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)

mapTree :: (a -> b) -> (Tree a -> Tree b)
