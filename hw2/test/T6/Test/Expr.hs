{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Expr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HW2.T4 (Expr(..), Prim(..))
import HW2.T1 (Except(..))
import HW2.T6 (ParseError(..))
import Numeric (showFFloat)
import Control.Monad (replicateM)


deriving instance (Eq a) => Eq (Prim a)
deriving instance Eq Expr
deriving instance Eq ParseError
deriving instance (Eq e, Eq a) => Eq (Except e a)

instance Show ParseError where
  show (ErrorAtPos n) = "at position: " ++ show n

instance Show Expr where
  show (Val x) = show x
  show (Op e) = show e

instance Show a => Show (Prim a) where
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x) = show "abs(" ++ show x ++ ")"
  show (Sgn x) = show "signum(" ++ show x ++ ")"

instance (Show e, Show a) => Show (Except e a) where
  show (Error e)   = "ERROR: " ++ show e
  show (Success a) = show a

genValInt :: Gen Expr
genValInt = Val . fromIntegral <$> Gen.int (Range.linear 0 10)

genVal :: Gen Expr
genVal = Val . read . flip (showFFloat (Just 5)) "" <$> Gen.double (Range.linearFrac 0 100)
-- genVal = Val <$> Gen.double (Range.linearFrac 0 100)

type OpCtr = Expr -> Expr -> Prim Expr

genExpr' :: Int -> [OpCtr] -> Gen Expr
genExpr' 0 _ = genVal
genExpr' depth ops = Gen.choice $ genVal : map genOps ops
  where
    genOps :: OpCtr -> Gen Expr
    genOps op =
      let nextExpr = genExpr' (depth - 1) ops
      in fmap Op $ op <$> nextExpr <*> nextExpr

genExpr :: [OpCtr] -> Gen Expr
genExpr ops = do
  depth <- Gen.int $ Range.linear 1 5
  genExpr' depth ops



showDouble :: Double -> String
showDouble = flip (showFFloat Nothing) ""

genSpaces :: Gen Int -> Gen String
genSpaces genN = do
  n <- genN
  return $ replicate n ' '

showBuilder' :: Expr -> Expr -> String -> Gen String -> Gen Int -> Gen String
showBuilder' a b op genSpaces genParen = do
  [s1, s2, s3, s4] <- replicateM 4 genSpaces
  bNum <- genParen
  as <- showBuilder a genSpaces genParen
  bs <- showBuilder b genSpaces genParen
  return $ replicate bNum '(' ++ s1 ++ as ++ s2 ++ op ++ s3 ++ bs ++ s4 ++ replicate  bNum ')'

showBuilder :: Expr -> Gen String -> Gen Int -> Gen String
showBuilder (Val x) = \_ _ -> return $ showDouble x
showBuilder (Op (Add a b)) = showBuilder' a b "+"
showBuilder (Op (Sub a b)) = showBuilder' a b "-"
showBuilder (Op (Mul a b)) = showBuilder' a b "*"
showBuilder (Op (Div a b)) = showBuilder' a b "/"
showBuilder _ = error "unreachable"

showFull :: Expr -> Gen String
showFull e = showBuilder e (genSpaces $ Gen.constant 0) (Gen.constant 1)

showExtra :: Expr -> Gen String
showExtra e = showBuilder e (genSpaces $ Gen.int $ Range.linear 0 2) (Gen.int $ Range.linear 1 2)

genExprBamboo' :: Int -> [OpCtr] -> Gen Expr
genExprBamboo' 0 _ = genVal
genExprBamboo' depth ops = Gen.choice $ map genOps ops
  where
    genOps :: OpCtr -> Gen Expr
    genOps op = fmap Op $ op <$> genExprBamboo' (depth - 1) ops <*> genVal

genExprBamboo :: [OpCtr] -> Gen Expr
genExprBamboo ops = do
  depth <- Gen.int $ Range.linear 1 10
  genExprBamboo' depth ops

showBamboo :: Expr -> Gen String
showBamboo e = showBuilder e (genSpaces $ Gen.constant 0) (Gen.constant 0)

-- ((((1 - 2) - (3 / 4 / 5)) - 6) - 7)
genExprPriority' :: Int -> Int -> Int -> Gen Expr
genExprPriority' 0 _ _ = genVal
genExprPriority' left middle right = do
  eLeft <- genExprBamboo' left [Sub]
  eMiddle <- genExprBamboo' middle [Div]
  eRight <- genExprBamboo' right [Sub]
  return $ replaceMostLeft eRight $ eLeft - eMiddle
    where
      replaceMostLeft :: Expr -> Expr -> Expr
      replaceMostLeft v@(Val _) _ = v
      replaceMostLeft (Op (Sub (Val _) r)) subst = subst - r
      replaceMostLeft (Op (Sub l r)) subst = replaceMostLeft l subst - r

genExprPriority :: Gen Expr
genExprPriority = do
  left <- Gen.int $ Range.linear 1 10
  middle <- Gen.int $ Range.linear 0 left
  right <- Gen.int $ Range.linear 0 middle
  genExprPriority' left middle right

showPriority :: Expr -> Gen String
showPriority e = showBuilder e (genSpaces $ Gen.constant 1) (Gen.constant 0)

genExprPriorityAssoc' :: Int -> Int -> Gen Expr
genExprPriorityAssoc' 0 _ = genValInt
genExprPriorityAssoc' depth thresh
  | depth <= thresh = genOps Mul
  | otherwise = genOps Add
  where
    genOps :: OpCtr -> Gen Expr
    genOps op =
      let nextExpr = genExprPriorityAssoc' (depth - 1) thresh
      in fmap Op $ op <$> nextExpr <*> nextExpr

genExprPriorityAssoc :: Gen Expr
genExprPriorityAssoc = do
  depth <- Gen.int $ Range.linear 1 7
  thresh <- Gen.int $ Range.linear 0 depth
  genExprPriorityAssoc' depth thresh

evalExpr' :: (Double -> Double -> Double) -> Expr -> Expr -> Double
evalExpr' f a b = f (evalExpr a) (evalExpr b)

evalExpr :: Expr -> Double
evalExpr (Val x) = x
evalExpr (Op (Add a b)) = evalExpr' (+) a b
evalExpr (Op (Sub a b)) = evalExpr' (-) a b
evalExpr (Op (Div a b)) = evalExpr' (/) a b
evalExpr (Op (Mul a b)) = evalExpr' (*) a b
