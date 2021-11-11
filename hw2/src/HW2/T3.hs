module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))

joinOption :: Option (Option a) -> Option a

joinExcept :: Except e (Except e a) -> Except e a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a

joinList :: List (List a) -> List a

joinFun :: Fun i (Fun i a) -> Fun i a
