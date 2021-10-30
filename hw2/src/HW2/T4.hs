module HW2.T4 where
import qualified Control.Monad
import HW2.T1

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
wrapState :: a -> State s a
joinState :: State s (State s a) -> State s a
modifyState :: (s -> s) -> State s ()
-- Using those functions, define Functor, Applicative, and Monad instances:

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)


-- These instances will enable the use of do-notation with State.

-- The semantics of State are such that the following holds:

-- runS (do modifyState f; modifyState g; return a) x
--   ≡
-- a :# g (f x)
-- In other words, we execute stateful actions left-to-right, passing the state from one to another.


data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)
-- For notational convenience, define the following instances:

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
--   ...
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where


-- So that (3.14 + 1.618 :: Expr) produces this syntax tree:

-- Op (Add (Val 3.14) (Val 1.618))
-- Using do-notation for State and combinators we defined for it (pure, modifyState), define the evaluation function:

eval :: Expr -> State [Prim Double] Double


-- In addition to the final result of evaluating an expression, it accumulates a trace of all individual operations:

-- runS (eval (2 + 3 * 5 - 7)) []
--   ≡
-- 10 :# [Sub 17 7, Add 2 15, Mul 3 5]
-- The head of the list is the last operation, this way adding another operation to the trace is O(1).

-- You can use the trace to observe the evaluation order. Consider this expression:

-- (a * b) + (x * y)
-- In eval, we choose to evaluate (a * b) first and (x * y) second, even though the opposite is also possible and
--  would not affect the final result of the computation.