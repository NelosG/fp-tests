module HW2.T3 where

import HW2.T1

joinOption    :: Option (Option a) -> Option a
joinExcept    :: Except e (Except e a) -> Except e a
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinList      :: List (List a) -> List a
joinFun       :: Fun i (Fun i a) -> Fun i a


-- The following laws must hold:

-- Associativity:

-- joinF (mapF joinF m)  ≡  joinF (joinF m)
-- In other words, given F (F (F a)), it does not matter whether we join the outer layers or the inner layers first.

-- Left and right identity:

-- joinF      (wrapF m)  ≡  m
-- joinF (mapF wrapF m)  ≡  m
-- In other words, layers created by wrapF are identity elements to joinF.

-- Given F a, you can add layers outside/inside to get F (F a), but joinF flattens it back into F a without any other changes to the structure.

-- Furthermore, joinF is strictly more powerful than distF and can be used to define it:

-- distF (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)
-- At the same time, this is only one of the possible distF definitions (e.g. List admits at least two lawful distF). 
-- It is common in Haskell to expect distF and joinF to agree in behavior, so the above equation must hold. 
-- (Do not redefine distF using joinF, though: it would be correct but not the point of the exercise).
