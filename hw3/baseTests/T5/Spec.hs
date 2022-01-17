{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments, TypeApplications #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import Data.Ratio ((%))
import Data.Fixed (Pico, showFixed)
import Data.Semigroup (stimes)
import Numeric.Natural (Natural)
import Data.Void (Void)
import Data.Char (isSpace)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.List (foldl', intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Functor.Classes (Eq1(liftEq))
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.String (renderString)
import qualified Language.Haskell.TH as TH

---------------------------
------ NAME CHECKING ------
---------------------------

import HW3.Base (
  HiFun (
    HiFunDiv,
    HiFunMul,
    HiFunAdd,
    HiFunSub,
    HiFunNot,
    HiFunAnd,
    HiFunOr,
    HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunIf,
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim,
    ---------
    HiFunList,
    HiFunRange,
    HiFunFold
  ))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList))
import HW3.Base (HiExpr(HiExprValue, HiExprApply))
import HW3.Base (HiError(HiErrorInvalidArgument, HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorDivideByZero))
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

import qualified HW3.Base

---------------------------
------ COMPATIBILITY ------
---------------------------

$(do mcls <- TH.lookupTypeName "HW3.Base.HiMonad"
     case mcls of
       Nothing -> return []
       Just cls -> do
         let stubClause = TH.clause [] (TH.normalB [e| error "Identity: runAction" |]) []
         let runActionD = TH.funD (TH.mkName "runAction") [stubClause]
         instD <- TH.instanceD (TH.cxt []) [t|$(TH.conT cls) Identity|] [runActionD]
         return [instD])

---------------------------
------ TYPE CHECKING ------
---------------------------

hiFunList' :: HiFun
hiFunList' = HiFunList

hiFunRange' :: HiFun
hiFunRange' = HiFunRange

hiFunFold' :: HiFun
hiFunFold' = HiFunFold

hiValueList' :: Seq HiValue -> HiValue
hiValueList' = HiValueList

hiErrorInvalidArgument' :: HiError
hiErrorInvalidArgument' = HiErrorInvalidArgument

hiErrorInvalidFunction' :: HiError
hiErrorInvalidFunction' = HiErrorInvalidFunction

hiErrorArityMismatch' :: HiError
hiErrorArityMismatch' = HiErrorArityMismatch

hiErrorDivideByZero' :: HiError
hiErrorDivideByZero' = HiErrorDivideByZero

eval' :: HiExpr -> Identity (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

data Target a = Target
  { getExpr :: HiExpr
  , getRes  :: HiValue
  , getStr  :: String
  , getAux  :: a
  }

instance Show (Target a) where
  show = getStr

toPico :: Real a => a -> Pico
toPico = realToFrac

fromPico :: Pico -> Rational
fromPico = toRational

size = 1000

rational :: QC.Gen (Target Rational)
rational = do
  it <- fromPico <$> QC.arbitrary
  return Target
    { getExpr = HiExprValue (HiValueNumber it)
    , getRes  = HiValueNumber it
    , getStr  = showFixed True (toPico it)
    , getAux  = it
    }

positiveInt :: QC.Gen (Target Rational, Int)
positiveInt = do
  QC.Positive i <- succ <$> QC.arbitrary @(QC.Positive Int)
  let it = toRational i
  return $ (,) Target
    { getExpr = HiExprValue (HiValueNumber it)
    , getRes  = HiValueNumber it
    , getStr  = showFixed True (toPico it)
    , getAux  = it
    }
    i

list :: QC.Gen (Target a) -> QC.Gen (Target a, Target [a], Target [a], [Target a])
list element = do
  x <- element
  xs <- QC.listOf element
  let
    res xs = Target
      { getExpr = applyFn HiFunList (map getExpr xs)
      , getRes  = HiValueList $ Seq.fromList $ map getRes xs
      , getStr  = "[" <> intercalate ", " (map getStr xs) <> "]"
      , getAux  = map getAux xs
      }

  return (x, res xs, res (x : xs), x : xs)

arith :: QC.Gen (Target (Rational -> Rational -> Rational))
arith = do
  (fun, hfun, sfun) <- QC.elements
    [ (HiFunAdd, (+), "add")
    , (HiFunSub, (-), "sub")
    , (HiFunMul, (*), "mul")
    , (HiFunDiv, (/), "div")
    ]
  return Target
    { getExpr = HiExprValue (HiValueFunction fun)
    , getRes  = HiValueFunction fun
    , getStr  = sfun
    , getAux  = hfun
    }

newtype Fold = Fold (Target Rational) deriving (Show)

instance QC.Arbitrary Fold where
  arbitrary = do
    fun <- arith
    (x, xs, full, _) <- list rational

    let res = foldl' (getAux fun) (getAux x) (getAux xs)

    return $ Fold Target
      { getExpr   = applyFn HiFunFold [getExpr fun, getExpr full]
      , getRes    = HiValueNumber res
      , getStr    = "fold(" <> getStr fun <> ", " <> getStr full <> ")"
      , getAux    = res
      }

prop_parse_well_formed_fold :: Fold -> Bool
prop_parse_well_formed_fold (Fold target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_fold :: Fold -> Bool
prop_eval_well_formed_fold (Fold target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Range = Range (Target [Rational]) deriving (Show)

instance QC.Arbitrary Range where
  arbitrary = do
    n <- rational
    m <- QC.chooseInt (0, size)
    let o = getAux n + fromIntegral m
    let res = [getAux n.. o]
    return $ Range Target
      { getExpr = applyFn HiFunRange [getExpr n, HiExprValue $ HiValueNumber o]
      , getStr  = "range(" <> getStr n <> ", " <> showFixed True (toPico o) <> ")"
      , getRes  = HiValueList $ Seq.fromList $ map HiValueNumber res
      , getAux  = res
      }

prop_parse_well_formed_range :: Range -> Bool
prop_parse_well_formed_range (Range target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_range :: Range -> Bool
prop_eval_well_formed_range (Range target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype List = List (Target [Rational]) deriving (Show)

instance QC.Arbitrary List where
  arbitrary = do
    (_, _, xs, _) <- list rational
    return (List xs)

prop_parse_well_formed_list :: List -> Bool
prop_parse_well_formed_list (List target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_list :: List -> Bool
prop_eval_well_formed_list (List target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Length = Length (Target Int) deriving (Show)

instance QC.Arbitrary Length where
  arbitrary = do
    (_, _, xs, _) <- list rational
    let len = length (getAux xs)
    return $ Length Target
      { getExpr = applyFn HiFunLength [getExpr xs]
      , getStr  = "length(" <> getStr xs <> ")"
      , getRes  = HiValueNumber (fromIntegral len)
      , getAux  = len
      }

prop_parse_well_formed_length :: Length -> Bool
prop_parse_well_formed_length (Length target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_length :: Length -> Bool
prop_eval_well_formed_length (Length target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Reverse = Reverse (Target [Rational]) deriving (Show)

instance QC.Arbitrary Reverse where
  arbitrary = do
    (_, _, xs, lst) <- list rational

    return $ Reverse Target
      { getExpr = applyFn HiFunReverse [getExpr xs]
      , getStr  = "reverse(" <> getStr xs <> ")"
      , getRes  = HiValueList $ Seq.fromList $ reverse (map getRes lst)
      , getAux  = reverse (getAux xs)
      }

prop_parse_well_formed_reverse :: Reverse -> Bool
prop_parse_well_formed_reverse (Reverse target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_reverse :: Reverse -> Bool
prop_eval_well_formed_reverse (Reverse target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Append = Append (Target [Rational]) deriving (Show)

instance QC.Arbitrary Append where
  arbitrary = do
    (_, _, xs, a) <- list rational
    (_, _, ys, b) <- list rational

    return $ Append Target
      { getExpr = applyFn HiFunAdd [getExpr xs, getExpr ys]
      , getStr  = getStr xs ++ " + " ++ getStr ys
      , getRes  = HiValueList $ Seq.fromList $ map getRes $ a ++ b
      , getAux  = getAux xs ++ getAux ys
      }

prop_parse_well_formed_append :: Append -> Bool
prop_parse_well_formed_append (Append target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_append :: Append -> Bool
prop_eval_well_formed_append (Append target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Repeat = Repeat (Target [Rational]) deriving (Show)

instance QC.Arbitrary Repeat where
  arbitrary = do
    (_, _, xs, a) <- list rational
    (y, yi) <- positiveInt
    let aux = stimes yi (map getRes a)
    return $ Repeat Target
      { getExpr = applyFn HiFunMul [getExpr xs, getExpr y]
      , getStr  = getStr xs ++ " * " ++ getStr y
      , getRes  = HiValueList $ Seq.fromList aux
      , getAux  = error "test suite bug: unreachable reached"
      }

prop_parse_well_formed_repeat :: Repeat -> Bool
prop_parse_well_formed_repeat (Repeat target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_repeat :: Repeat -> Bool
prop_eval_well_formed_repeat (Repeat target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Index = Index (Target Rational) deriving (Show)

instance QC.Arbitrary Index where
  arbitrary = do
    (_, _, xs, ys) <- list rational
    (y, yi) <- positiveInt
    let s   = map getRes ys
    let res = if yi >= length s then HiValueNull else s !! yi
    return $ Index Target
      { getExpr = HiExprApply (applyFn HiFunList (map getExpr ys)) [getExpr y]
      , getStr  = getStr xs ++ "(" ++ getStr y ++ ")"
      , getRes  = res
      , getAux  = error "test suite bug: unreachable reached"
      }

prop_parse_well_formed_index :: Index -> Bool
prop_parse_well_formed_index (Index target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_index :: Index -> Bool
prop_eval_well_formed_index (Index target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

newtype Slice = Slice (Target Rational) deriving (Show)

instance QC.Arbitrary Slice where
  arbitrary = do
    (_, _, xs, ys) <- list rational
    (y, yi) <- positiveInt
    (d, di) <- positiveInt
    let zi = yi + di
    let z  = HiExprValue $ HiValueNumber $ fromIntegral zi
    let s   = map getRes ys
    let res = take di $ drop yi s
    return $ Slice Target
      { getExpr = HiExprApply (applyFn HiFunList (map getExpr ys)) [getExpr y, z]
      , getStr  = getStr xs ++ "(" ++ getStr y ++ ", " ++ show zi ++ ")"
      , getRes  = HiValueList $ Seq.fromList res
      , getAux  = error "test suite bug: unreachable reached"
      }

prop_parse_well_formed_slice :: Slice -> Bool
prop_parse_well_formed_slice (Slice target) =
  getStr target `parses_to` getExpr target

prop_eval_well_formed_slice :: Slice -> Bool
prop_eval_well_formed_slice (Slice target) =
  runIdentity (eval (getExpr target))
    `eqResult`
  return (getRes target)

applyFn :: HiFun -> [HiExpr] -> HiExpr
applyFn fn args = HiExprApply (HiExprValue (HiValueFunction fn)) args

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "list"      `parses_to` HiExprValue (HiValueFunction HiFunList)  &&
  "fold"      `parses_to` HiExprValue (HiValueFunction HiFunFold)  &&
  "range"     `parses_to` HiExprValue (HiValueFunction HiFunRange) &&
  "[1, false, \"hello\", []]" `parses_to` applyFn HiFunList
    [
      HiExprValue (HiValueNumber 1),
      HiExprValue (HiValueBool False),
      HiExprValue (HiValueString (Text.pack "hello")),
      applyFn HiFunList []
    ]

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
   "list(1, 2, 3)"  `evaluates_to_list_num` [1, 2, 3] &&
   "range(5, 10.3)" `evaluates_to_list_num` [5, 6, 7, 8, 9, 10] &&
   "fold(add, [11, 22, 33])" `evaluates_to_num` 66      &&
   "fold(mul, [11, 22, 33])" `evaluates_to_num` 7986    &&
   "fold(div, [11, 22, 33])" `evaluates_to_num` (1%66)  &&
   "length([1, true, \"Hello\"])"  `evaluates_to_num` 3 &&
   "reverse([1, true, \"Hello\"])" `evaluates_to_list`
      [
        HiValueString (Text.pack "Hello"),
        HiValueBool True,
        HiValueNumber 1
      ] &&
   "[1, 2] + [3, 4, 5]" `evaluates_to_list_num` [1, 2, 3, 4, 5] &&
   "[0, \"x\"] * 3" `evaluates_to_list`
      [
        HiValueNumber 0, HiValueString (Text.pack "x"),
        HiValueNumber 0, HiValueString (Text.pack "x"),
        HiValueNumber 0, HiValueString (Text.pack "x")
      ] &&
   "[\"hello\", true, \"world\"](1)" `evaluates_to_bool` True &&
   "[\"hello\", true, \"world\"](1,3)" `evaluates_to_list` [HiValueBool True, HiValueString (Text.pack "world")] &&
   "fold(add, [2, 5] * 3)" `evaluates_to_num` 21 &&
   "fold(mul, range(1, 10))" `evaluates_to_num` 3628800 &&
   "[0, true, false, \"hello\", \"world\"](2, 4)" `evaluates_to_list`
      [
        HiValueBool False,
        HiValueString (Text.pack "hello")
      ] &&
   "reverse(range(0.5, 70/8))" `evaluates_to_list_num`
      [ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]

filterOutSpaces :: String -> String
filterOutSpaces = filter (not . isSpace)

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left e -> False
    Right e -> eqExpr e expected

evaluates_to :: String -> HiValue -> Bool
evaluates_to str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v expected

evaluates_to_num :: String -> Rational -> Bool
evaluates_to_num str expected = str `evaluates_to` HiValueNumber expected

evaluates_to_bool :: String -> Bool -> Bool
evaluates_to_bool str expected = str `evaluates_to` HiValueBool expected

evaluates_to_list :: String -> [HiValue] -> Bool
evaluates_to_list str expected = str `evaluates_to` HiValueList (Seq.fromList expected)

evaluates_to_list_num :: String -> [Rational] -> Bool
evaluates_to_list_num str expected = str `evaluates_to_list` map HiValueNumber expected

eqFn :: HiFun -> HiFun -> Bool
eqFn HiFunDiv HiFunDiv = True
eqFn HiFunMul HiFunMul = True
eqFn HiFunAdd HiFunAdd = True
eqFn HiFunSub HiFunSub = True
eqFn HiFunNot HiFunNot = True
eqFn HiFunAnd HiFunAnd = True
eqFn HiFunOr HiFunOr = True
eqFn HiFunLessThan HiFunLessThan = True
eqFn HiFunGreaterThan HiFunGreaterThan = True
eqFn HiFunEquals HiFunEquals = True
eqFn HiFunNotLessThan HiFunNotLessThan = True
eqFn HiFunNotGreaterThan HiFunNotGreaterThan = True
eqFn HiFunNotEquals HiFunNotEquals = True
eqFn HiFunIf HiFunIf = True
eqFn HiFunLength HiFunLength = True
eqFn HiFunToUpper HiFunToUpper = True
eqFn HiFunToLower HiFunToLower = True
eqFn HiFunReverse HiFunReverse = True
eqFn HiFunTrim HiFunTrim = True
eqFn HiFunList HiFunList = True
eqFn HiFunRange HiFunRange = True
eqFn HiFunFold HiFunFold = True
eqFn _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
eqValue (HiValueString s1) (HiValueString s2) = s1 == s2
eqValue (HiValueList xs1) (HiValueList xs2) = liftEq eqValue xs1 xs2
eqValue HiValueNull HiValueNull = True
eqValue _ _ = False

eqExpr :: HiExpr -> HiExpr -> Bool
eqExpr (HiExprApply fn1 args1) (HiExprApply fn2 args2) =
  eqExpr fn1 fn2 && liftEq eqExpr args1 args2
eqExpr (HiExprValue v1) (HiExprValue v2) =
  eqValue v1 v2
eqExpr _ _ = False

eqError :: HiError -> HiError -> Bool
eqError HiErrorInvalidArgument HiErrorInvalidArgument = True
eqError HiErrorInvalidFunction HiErrorInvalidFunction = True
eqError HiErrorArityMismatch HiErrorArityMismatch = True
eqError HiErrorDivideByZero HiErrorDivideByZero = True
eqError _ _ = False

eqResult :: Either HiError HiValue -> Either HiError HiValue -> Bool
eqResult (Left err1) (Left err2) = eqError err1 err2
eqResult (Right v1) (Right v2) = eqValue v1 v2
eqResult _ _ = False

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
