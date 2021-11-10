{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Expr
  ( evalExprInt
  , genFullExpr
  , stupidEval
  ) where

import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.List (intercalate, sort)
import HW2.T1 (Annotated (..))
import HW2.T4 (Expr (..), Prim (..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric (showFFloat)

instance Eq (Prim Double) where
  (==) (Add a b) (Add c d) = (isNaN a && isNaN c || a == c) && (isNaN b && isNaN d || b == d)
  (==) (Sub a b) (Sub c d) = (isNaN a && isNaN c || a == c) && (isNaN b && isNaN d || b == d)
  (==) (Mul a b) (Mul c d) = (isNaN a && isNaN c || a == c) && (isNaN b && isNaN d || b == d)
  (==) (Div a b) (Div c d) = (isNaN a && isNaN c || a == c) && (isNaN b && isNaN d || b == d)

  (==) (Abs a) (Abs b)     = isNaN a && isNaN b || a == b
  (==) (Sgn a) (Sgn b)     = isNaN a && isNaN b || a == b

  (==) _ _                 = False

deriving instance Eq (Prim Expr)

instance Eq Expr where
  (==) (Val a) (Val b) = isNaN a && isNaN b || a == b
  (==) (Op a) (Op b)   = a == b
  (==) _ _             = False

deriving instance Ord (Prim Double)

instance Eq (Annotated [Prim Double] Double) where
  (==) (a :# aPrim) (b :# bPrim) = (isNaN a && isNaN b || a == b) && sort aPrim == sort bPrim

deriving instance Eq (Annotated [Prim Expr] Double)
deriving instance (Show a, Show e) => Show (Annotated e a)

instance Show Expr where
  show (Val x) = show x
  show (Op e)  = show e

instance Show a => Show (Prim a) where
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x)   = "abs(" ++ show x ++ ")"
  show (Sgn x)   = "signum(" ++ show x ++ ")"

genValInt :: Gen Expr
genValInt = Val . fromIntegral <$> Gen.int (Range.linear 1 10)

genVal :: Gen Expr
genVal = Val . read . flip (showFFloat (Just 5)) "" <$> Gen.double (Range.linearFrac 0 100)

type OpCtr = Expr -> Expr -> Prim Expr
type UnaryOpCtr = Expr -> Prim Expr

genExprBuilder' :: Int -> Bool -> [OpCtr] -> [UnaryOpCtr] -> Gen Expr
genExprBuilder' 0 _ _ _ = genVal
genExprBuilder' depth valInMiddle ops unaryOps = Gen.choice $
  (if valInMiddle then (genVal :) else id) $ map genOps ops ++ map genUnaryOps unaryOps
  where
    nextExpr :: Gen Expr
    nextExpr = genExprBuilder' (depth - 1) valInMiddle ops unaryOps

    genOps :: OpCtr -> Gen Expr
    genOps op = fmap Op $ op <$> nextExpr <*> nextExpr

    genUnaryOps :: UnaryOpCtr -> Gen Expr
    genUnaryOps op = fmap Op $ op <$> nextExpr


genExprBuilder :: Gen Int -> Bool -> [OpCtr] -> [UnaryOpCtr] -> Gen Expr
genExprBuilder genDepth valInMiddle ops unaryOps = do
  depth <- genDepth
  genExprBuilder' depth valInMiddle ops unaryOps

genExpr :: [OpCtr] -> [UnaryOpCtr] -> Gen Expr
genExpr = genExprBuilder (Gen.int $ Range.linear 1 5) True

genFullExpr :: Gen Expr
genFullExpr = genExpr [Add, Mul, Sub, Div] [Abs, Sgn]

evalExprInt' :: (Double -> Double -> Double) -> Expr -> Expr -> Double
evalExprInt' f a b = f (evalExprInt a) (evalExprInt b)

evalExprInt'' :: (Double -> Double) -> Expr -> Double
evalExprInt'' f a = f (evalExprInt a)

evalExprInt :: Expr -> Double
evalExprInt (Val x)        = x
evalExprInt (Op (Add a b)) = evalExprInt' (+) a b
evalExprInt (Op (Sub a b)) = evalExprInt' (-) a b
evalExprInt (Op (Div a b)) = evalExprInt' (/) a b
evalExprInt (Op (Mul a b)) = evalExprInt' (*) a b
evalExprInt (Op (Abs a))   = evalExprInt'' abs a
evalExprInt (Op (Sgn a))   = evalExprInt'' signum a

-- Please, do not refactor it.
stupidEval :: Expr -> [Prim Double] -> Annotated [Prim Double] Double
stupidEval (Val x)        st = x :# st
stupidEval (Op (Add a b)) st =
  let resA :# stA = stupidEval a st
      resB :# stB = stupidEval b stA
  in evalExprInt' (+) a b :# (Add resA resB) : stB
stupidEval (Op (Sub a b)) st =
  let resA :# stA = stupidEval a st
      resB :# stB = stupidEval b stA
  in evalExprInt' (-) a b :# (Sub resA resB) : stB
stupidEval (Op (Div a b)) st =
  let resA :# stA = stupidEval a st
      resB :# stB = stupidEval b stA
  in evalExprInt' (/) a b :# (Div resA resB) : stB
stupidEval (Op (Mul a b)) st =
  let resA :# stA = stupidEval a st
      resB :# stB = stupidEval b stA
  in evalExprInt' (*) a b :# (Mul resA resB) : stB
stupidEval (Op (Abs a))   st =
  let resA :# stA = stupidEval a st
  in evalExprInt'' abs a :# (Abs resA) : stA
stupidEval (Op (Sgn a))   st =
  let resA :# stA = stupidEval a st
  in evalExprInt'' signum a :# (Sgn resA) : stA
