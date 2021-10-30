module HW2.T1 where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a = Nil | a :. List a
infixr 5 :.

data Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)


mapOption      :: (a -> b) -> (Option a -> Option b)
mapPair        :: (a -> b) -> (Pair a -> Pair b)
mapQuad        :: (a -> b) -> (Quad a -> Quad b)
mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
mapExcept      :: (a -> b) -> (Except e a -> Except e b)
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapStream      :: (a -> b) -> (Stream a -> Stream b)
mapList        :: (a -> b) -> (List a -> List b)
mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
mapTree        :: (a -> b) -> (Tree a -> Tree b)


-- These functions must modify only the elements and preserve the overall structure (e.g. do not reverse the list, do not rebalance the tree, do not swap the pair).

-- This property is witnessed by the following laws:

--          mapF id  ≡  id
-- mapF f ∘ mapF g   ≡  mapF (f ∘ g)
-- You must implement these functions by hand, without using any predefined functions (not even from Prelude) or deriving.