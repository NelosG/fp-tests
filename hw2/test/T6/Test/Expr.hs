{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Expr where

import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.List (intercalate)
import HW2.T1 (Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T6 (ParseError (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric (showFFloat)


deriving instance (Eq a) => Eq (Prim a)
deriving instance Eq ParseError
deriving instance (Eq e, Eq a) => Eq (Except e a)

instance Eq Expr where
  (Val a) == (Val b) = abs (a - b) < 0.0000001
  (Op p) == (Op q)   = p == q
  (==) _ _           = False

instance Show ParseError where
  show (ErrorAtPos n) = "at position: " ++ show n

instance Show Expr where
  show (Val x) = show x
  show (Op e)  = show e

instance Show a => Show (Prim a) where
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x)   = show "abs(" ++ show x ++ ")"
  show (Sgn x)   = show "signum(" ++ show x ++ ")"

instance (Show e, Show a) => Show (Except e a) where
  show (Error e)   = "ERROR: " ++ show e
  show (Success a) = show a

genValInt :: Gen Expr
genValInt = Val . fromIntegral <$> Gen.int (Range.linear 1 10)

genVal :: Gen Expr
genVal = Val . read . flip (showFFloat (Just 5)) "" <$> Gen.double (Range.linearFrac 0 100)
-- genVal = Val <$> Gen.double (Range.linearFrac 0 100)

type OpCtr = Expr -> Expr -> Prim Expr

genExprBuilder' :: Int -> Bool -> [OpCtr] -> Gen Expr
genExprBuilder' 0 _ _ = genVal
genExprBuilder' depth valInMiddle ops = Gen.choice $
  (if valInMiddle then (genVal :) else id) $ map genOps ops
  where
    genOps :: OpCtr -> Gen Expr
    genOps op =
      let nextExpr = genExprBuilder' (depth - 1) valInMiddle ops
      in fmap Op $ op <$> nextExpr <*> nextExpr

genExprBuilder :: Gen Int -> Bool -> [OpCtr] -> Gen Expr
genExprBuilder genDepth valInMiddle ops = do
  depth <- genDepth
  genExprBuilder' depth valInMiddle ops

genExpr :: [OpCtr] -> Gen Expr
genExpr = genExprBuilder (Gen.int $ Range.linear 1 5) True

showDouble :: Double -> String
showDouble = flip (showFFloat Nothing) ""

genSpaces :: Gen Int -> Gen String
genSpaces genN = do
  n <- genN
  return $ replicate n ' '

showBuilder' :: Expr -> Expr -> String -> Gen String -> Gen Int -> Gen Bool -> Gen String
showBuilder' a b op genSpaces genParen genInvalid = do
  [s1, s2, s3, s4, sParen] <- replicateM 5 genSpaces
  bNum <- genParen
  as <- showBuilder a genSpaces genParen genInvalid
  bs <- showBuilder b genSpaces genParen genInvalid
  [e1, e2, e3, e4, e5, e6, e7, e8, eBegin, eEnd] <- map (bool "" "LOL") <$> replicateM 10 genInvalid
  return $ eBegin ++ intercalate sParen (replicate bNum "(")
    ++ e1 ++ s1 ++ e2 ++ as ++ e3 ++ s2 ++ e4 ++ op ++ e5 ++ s3 ++ e6 ++ bs ++ e7 ++ s4 ++ e8
    ++ intercalate sParen (replicate bNum ")") ++ eEnd

showBuilder :: Expr -> Gen String -> Gen Int -> Gen Bool -> Gen String
showBuilder (Val x)        = \_ _ _ -> return $ showDouble x
showBuilder (Op (Add a b)) = showBuilder' a b "+"
showBuilder (Op (Sub a b)) = showBuilder' a b "-"
showBuilder (Op (Mul a b)) = showBuilder' a b "*"
showBuilder (Op (Div a b)) = showBuilder' a b "/"
showBuilder _              = error "unreachable"

showFull :: Expr -> Gen String
showFull e = showBuilder e (genSpaces $ Gen.constant 0) (Gen.constant 1) (Gen.constant False)

showExtra :: Expr -> Gen String
showExtra e = showBuilder e (genSpaces $ Gen.int $ Range.linear 0 2) (Gen.int $ Range.linear 1 2) (Gen.constant False)

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
showBamboo e = showBuilder e (genSpaces $ Gen.constant 0) (Gen.constant 0) (Gen.constant False)

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
      replaceMostLeft v@(Val _) _                = v
      replaceMostLeft (Op (Sub (Val _) r)) subst = subst - r
      replaceMostLeft (Op (Sub l r)) subst       = replaceMostLeft l subst - r

genExprPriority :: Gen Expr
genExprPriority = do
  left <- Gen.int $ Range.linear 1 10
  middle <- Gen.int $ Range.linear 0 left
  right <- Gen.int $ Range.linear 0 middle
  genExprPriority' left middle right

showPriority :: Expr -> Gen String
showPriority e = showBuilder e (genSpaces $ Gen.constant 1) (Gen.constant 0) (Gen.constant False)

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

evalExprInt' :: (Double -> Double -> Double) -> Expr -> Expr -> Double
evalExprInt' f a b = fromIntegral $ (`mod` 133711) . round $ f (evalExprInt a) (evalExprInt b)

evalExprInt :: Expr -> Double
evalExprInt (Val x)        = x
evalExprInt (Op (Add a b)) = evalExprInt' (+) a b
evalExprInt (Op (Sub a b)) = evalExprInt' (-) a b
evalExprInt (Op (Div a b)) = evalExprInt' (/) a b
evalExprInt (Op (Mul a b)) = evalExprInt' (*) a b


convertToLeftAssoc :: Expr -> Expr
convertToLeftAssoc = convertToLeftAssoc' id

convertToLeftAssoc' :: (Expr -> Expr) -> Expr -> Expr
convertToLeftAssoc' b v@(Val _) = b v
convertToLeftAssoc' b (Op (Add l r)) =
  let
    left =
      case l of
        (Op (Mul _ _))  -> b $ convertToLeftAssoc' id l
        (Op (Div _ _ )) -> b $ convertToLeftAssoc' id l
        _               -> convertToLeftAssoc' b l
  in case r of
    (Op (Mul _ _))  -> Op $ Add left $ convertToLeftAssoc' id r
    (Op (Div _ _ )) -> Op $ Add left $ convertToLeftAssoc' id r
    _               -> convertToLeftAssoc' (Op . Add left) r
convertToLeftAssoc' b (Op (Mul l r)) =
  let left = convertToLeftAssoc' b l
  in convertToLeftAssoc' (Op . Mul left) r
convertToLeftAssoc' b (Op (Sub l r)) =
  let
    left =
      case l of
        (Op (Mul _ _))  -> b $ convertToLeftAssoc' id l
        (Op (Div _ _ )) -> b $ convertToLeftAssoc' id l
        _               -> convertToLeftAssoc' b l
  in Op $ Sub left (convertToLeftAssoc' id r)
convertToLeftAssoc' b (Op (Div l r)) =
  let left = convertToLeftAssoc' b l
  in Op $ Div left (convertToLeftAssoc' id r)


type Wrapper = (String -> String)

showMin' :: Wrapper -> Wrapper -> Expr -> Expr -> String -> String
showMin' wrapL wrapR l r op = wrapL (showMin l) ++ op ++ wrapR (showMin r)

wrapParen :: String -> String
wrapParen = ("("++) . (++")")

wrapAddSub' :: Expr -> Wrapper
wrapAddSub' = \case
  (Op (Add _ _)) -> wrapParen
  (Op (Sub _ _)) -> wrapParen
  _              -> id

showMin :: Expr -> String
showMin (Val v) = showDouble v
showMin (Op (Add l r)) = showMin' id id l r "+"
showMin (Op (Sub l r)) = showMin' id (wrapAddSub' r) l r "-"
showMin (Op (Mul l r)) = showMin' (wrapAddSub' l) (wrapAddSub' r) l r "*"
showMin (Op (Div l r)) = showMin'
  (wrapAddSub' l)
  case r of
    (Val _) -> id
    _       -> wrapParen
  l r "/"

showMinGen :: Expr -> Gen String
showMinGen = Gen.constant . showMin

showInvalidExpr :: Expr -> Gen String
showInvalidExpr e = showBuilder e (genSpaces $ Gen.constant 0) (Gen.constant 1) Gen.bool_

genExprInvalid :: [OpCtr] -> Gen Expr
genExprInvalid = genExprBuilder (Gen.int $ Range.linear 2 5) False
