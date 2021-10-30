module HW2.T5 where

import HW2.T4

-- Implement the following functions:

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
wrapExceptState :: a -> ExceptState e s a
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
modifyExceptState :: (s -> s) -> ExceptState e s ()
throwExceptState :: e -> ExceptState e s a
-- Using those functions, define Functor, Applicative, and Monad instances.

-- Using do-notation for ExceptState and combinators we defined for it (pure, modifyExceptState, throwExceptState), define the evaluation function:

data EvaluationError = DivideByZero
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
-- It works just as eval from the previous task but aborts the computation if division by zero occurs:

--   runES (eval (2 + 3 * 5 - 7)) []
--     ≡
--   Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])
--   runES (eval (1 / (10 - 5 * 2))) []
--     ≡
--   Error DivideByZero