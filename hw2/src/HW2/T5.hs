module HW2.T5
  ( -- * Datatypes
    EvaluationError (..)
  , ExceptState (..)
    -- * map functions
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , wrapExceptState
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (Op, Val), Prim (Abs, Add, Div, Mul, Sgn, Sub))

data ExceptState e s a = ES
  { runES :: s -> Except e (Annotated s a)
  }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b

wrapExceptState :: a -> ExceptState e s a

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a

modifyExceptState :: (s -> s) -> ExceptState e s ()

throwExceptState :: e -> ExceptState e s a

instance Functor (...) where
  ...

instance Applicative (...) where
  ..

instance Monad (...) where
  ...

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
