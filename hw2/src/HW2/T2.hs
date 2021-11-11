module HW2.T2
  ( -- * dist functions
    distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
    -- * wrap functions
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))
import Prelude (Monoid, Semigroup, mempty, (<>))

distOption :: (Option a, Option b) -> Option (a, b)

distPair :: (Pair a, Pair b) -> Pair (a, b)

distQuad :: (Quad a, Quad b) -> Quad (a, b)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)

distExcept :: (Except e a, Except e b) -> Except e (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)

distList :: (List a, List b) -> List (a, b)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)

wrapOption :: a -> Option a

wrapPair :: a -> Pair a

wrapQuad :: a -> Quad a

wrapAnnotated   :: Monoid e => a -> Annotated e a

wrapExcept :: a -> Except e a

wrapPrioritised :: a -> Prioritised a

wrapStream :: a -> Stream a

wrapList :: a -> List a

wrapFun :: a -> Fun i a
