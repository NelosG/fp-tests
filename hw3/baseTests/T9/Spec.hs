{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments,
             StandaloneKindSignatures, ConstraintKinds, GeneralizedNewtypeDeriving,
             DerivingStrategies, FlexibleInstances, BangPatterns #-}

import Control.Monad (unless, guard)
import System.Exit (exitFailure)
import Text.Megaparsec (ParseErrorBundle)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Ratio (numerator, denominator)
import Data.Text (Text)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Kind (Type, Constraint)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Classes (Eq1(liftEq))
import Data.Functor.Identity (Identity(runIdentity))
import Control.Monad.Trans.Maybe
import Data.Maybe (isJust)
import System.Directory (removePathForcibly)
import Control.Exception (catch)

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
    HiFunList,
    HiFunRange,
    HiFunFold,
    HiFunPackBytes,
    HiFunUnpackBytes,
    HiFunEncodeUtf8,
    HiFunDecodeUtf8,
    HiFunZip,
    HiFunUnzip,
    HiFunSerialise,
    HiFunDeserialise,
    ---------
    HiFunRand
  ))
import HW3.Base (HiAction (HiActionRand))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList, HiValueBytes, HiValueAction))
import HW3.Base (HiExpr(HiExprValue, HiExprApply, HiExprRun))
import HW3.Base (HiError(HiErrorInvalidArgument, HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorDivideByZero))
import HW3.Base (HiMonad(runAction))
import HW3.Action (HIO(HIO, runHIO))
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

---------------------------
------ TYPE CHECKING ------
---------------------------

hiActionRand' :: Int -> Int -> HiAction
hiActionRand' = HiActionRand

type HiMonad' :: (Type -> Type) -> Constraint
type HiMonad' = HiMonad

runAction' :: HiMonad m => HiAction -> m HiValue
runAction' = runAction

hiFunRand' :: HiFun
hiFunRand' = HiFunRand

hiValueAction' :: HiAction -> HiValue
hiValueAction' = HiValueAction

hiExprRun' :: HiExpr -> HiExpr
hiExprRun' = HiExprRun

eval' :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "rand" `parses_to` HiExprValue (HiValueFunction HiFunRand) &&
  "rand(0, 10)!" `parses_to` HiExprRun (applyFn HiFunRand [HiExprValue (HiValueNumber 0), HiExprValue (HiValueNumber 10)])

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "rand(0, 10)" `evaluates_to_act` HiActionRand 0 10

data T3 = X | Y | Z

data Count3 = C3 !Int !Int !Int

countT3 :: T3 -> Count3 -> Count3
countT3 X (C3 x y z) = C3 (x+1) y z
countT3 Y (C3 x y z) = C3 x (y+1) z
countT3 Z (C3 x y z) = C3 x y (z+1)

prop_rand_distrib :: QC.Property
prop_rand_distrib =
  QC.ioProperty $ fmap isJust $ runMaybeT do
    let sampleSize = 10000
    C3 x y z <- genItems sampleSize (C3 0 0 0)
    MaybeT (fmap Just (print (x, y, z)))
    let expected = sampleSize `div` 3
        inRange n = abs (n - expected) < (expected `div` 20) -- within 5% of uniform distrib
    guard (inRange x && inRange y && inRange z)
  where
    genItems :: Int -> Count3 -> MaybeT IO Count3
    genItems 0 !c = return c
    genItems k !c = do
      item <- genItem
      genItems (k - 1) (countT3 item c)

    genItem :: MaybeT IO T3
    genItem = MaybeT do
      case parse "rand(0, 2)!" of
        Left _ -> return Nothing
        Right e -> do
          res <- runHIO (eval e) Set.empty
          return $ case res of
            Left  _ -> Nothing
            Right (HiValueNumber x) -> do
              guard (denominator x == 1)
              case numerator x of
                0 -> Just X
                1 -> Just Y
                2 -> Just Z
                _ -> Nothing
            Right _ -> Nothing

applyFn :: HiFun -> [HiExpr] -> HiExpr
applyFn fn args = HiExprApply (HiExprValue (HiValueFunction fn)) args

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

evaluates_to_act :: String -> HiAction -> Bool
evaluates_to_act str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runFakeRand (eval e) of
        Left _  -> False
        Right v -> eqValue v (HiValueAction expected)

newtype FakeRand a = FakeRand (Identity a)
  deriving newtype (Functor, Applicative, Monad)

runFakeRand :: FakeRand a -> a
runFakeRand (FakeRand m) = runIdentity m

instance HiMonad FakeRand where
  runAction act = do
    case act of
      HiActionRand n _ -> return (HiValueNumber (fromIntegral n))
      _ -> error "FakeRand: unsupported action"

eqResult :: Either HiError HiValue -> Either HiError HiValue -> Bool
eqResult (Left err1) (Left err2) = eqError err1 err2
eqResult (Right v1) (Right v2) = eqValue v1 v2
eqResult _ _ = False

eqError :: HiError -> HiError -> Bool
eqError HiErrorInvalidArgument HiErrorInvalidArgument = True
eqError HiErrorInvalidFunction HiErrorInvalidFunction = True
eqError HiErrorArityMismatch HiErrorArityMismatch = True
eqError HiErrorDivideByZero HiErrorDivideByZero = True
eqError _ _ = False

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
eqFn HiFunPackBytes HiFunPackBytes = True
eqFn HiFunUnpackBytes HiFunUnpackBytes = True
eqFn HiFunEncodeUtf8 HiFunEncodeUtf8 = True
eqFn HiFunDecodeUtf8 HiFunDecodeUtf8 = True
eqFn HiFunZip HiFunZip = True
eqFn HiFunUnzip HiFunUnzip = True
eqFn HiFunSerialise HiFunSerialise = True
eqFn HiFunDeserialise HiFunDeserialise = True
eqFn HiFunRand HiFunRand = True
eqFn _ _ = False

eqAction :: HiAction -> HiAction -> Bool
eqAction (HiActionRand n1 m1) (HiActionRand n2 m2) = n1 == n2 && m1 == m2
eqAction _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
eqValue (HiValueString s1) (HiValueString s2) = s1 == s2
eqValue (HiValueList xs1) (HiValueList xs2) = xs1 == xs2
eqValue (HiValueBytes bs1) (HiValueBytes bs2) = bs1 == bs2
eqValue HiValueNull HiValueNull = True
eqValue (HiValueAction act1) (HiValueAction act2) = eqAction act1 act2
eqValue _ _ = False

eqExpr :: HiExpr -> HiExpr -> Bool
eqExpr (HiExprApply fn1 args1) (HiExprApply fn2 args2) =
  eqExpr fn1 fn2 && liftEq eqExpr args1 args2
eqExpr (HiExprValue v1) (HiExprValue v2) =
  eqValue v1 v2
eqExpr (HiExprRun e1) (HiExprRun e2) =
  eqExpr e1 e2
eqExpr _ _ = False

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
