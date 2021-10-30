module HW2.T2 where

import HW2.T1

distOption      :: (Option a, Option b) -> Option (a, b)
distPair        :: (Pair a, Pair b) -> Pair (a, b)
distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distStream      :: (Stream a, Stream b) -> Stream (a, b)
distList        :: (List a, List b) -> List (a, b)
distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
wrapOption      :: a -> Option a
wrapPair        :: a -> Pair a
wrapQuad        :: a -> Quad a
wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapExcept      :: a -> Except e a
wrapPrioritised :: a -> Prioritised a
wrapStream      :: a -> Stream a
wrapList        :: a -> List a
wrapFun         :: a -> Fun i a


-- The following laws must hold:

-- Homomorphism:

-- distF (wrapF a, wrapF b)  ≅  wrapF (a, b)
-- Associativity:

-- distF (p, distF (q, r))   ≅  distF (distF (p, q), r)
-- Left and right identity:

-- distF (wrapF (), q)  ≅  q
-- distF (p, wrapF ())  ≅  p

-- In the laws stated above, we reason up to the following isomorphisms:

-- ((a, b), c)  ≅  (a, (b, c))  -- for associativity
--     ((), b)  ≅  b            -- for left identity
--     (a, ())  ≅  a            -- for right identity
-- There is more than one way to implement some of these functions. In addition to the laws, take the following expectations into account:

-- distPrioritised must pick the higher priority out of the two.
-- distList must associate each element of the first list with each element of the second list (i.e. the resulting list is of length n × m).
-- You must implement these functions by hand, using only:

-- data types that you defined in HW.T1
-- <> and mempty for Annotated
