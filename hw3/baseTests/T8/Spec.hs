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
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

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
    HiFunParseTime
  ))
import HW3.Base (HiAction (HiActionNow))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList, HiValueBytes, HiValueAction, HiValueTime))
import HW3.Base (HiExpr(HiExprValue, HiExprApply, HiExprRun))
import HW3.Base (HiError(HiErrorInvalidArgument, HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorDivideByZero))
import HW3.Base (HiMonad(runAction))
import HW3.Action (HiPermission(AllowRead, AllowWrite, AllowTime))
import HW3.Action (PermissionException(PermissionRequired))
import HW3.Action (HIO(HIO, runHIO))
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

---------------------------
------ TYPE CHECKING ------
---------------------------

hiActionNow' :: HiAction
hiActionNow' = HiActionNow

type HiMonad' :: (Type -> Type) -> Constraint
type HiMonad' = HiMonad

runAction' :: HiMonad m => HiAction -> m HiValue
runAction' = runAction

hiFunParseTime' :: HiFun
hiFunParseTime' = HiFunParseTime

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
  "parse-time" `parses_to` HiExprValue (HiValueFunction HiFunParseTime) &&
  "now"        `parses_to` HiExprValue (HiValueAction HiActionNow)

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000"
    `evaluates_to` HiValueTime (readTime "2021-12-15 00:16:40 UTC")
  &&
  "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
    `evaluates_to` HiValueNumber 3.351843755
  &&
  "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60"
    `evaluates_to` HiValueTime (readTime "2022-01-01 00:00:00 UTC")

prop_sys_time :: QC.Property
prop_sys_time =
  QC.ioProperty $ fmap isJust $ runMaybeT do
    HiValueTime t1 <- MaybeT $ execHiCmd "now!" (Set.singleton AllowTime)
    t2 <- MaybeT $ fmap Just getCurrentTime
    guard (diffUTCTime t2 t1 < 1) -- less than 1 second off

execHiCmd :: String -> Set HiPermission -> IO (Maybe HiValue)
execHiCmd str permissions = do
  case parse str of
    Left _ -> return Nothing
    Right e -> do
      res <- runHIO (eval e) permissions
      return $ case res of
        Left  _ -> Nothing
        Right v -> Just v

prop_io_permissions :: QC.Property
prop_io_permissions =
  QC.ioProperty $ fmap isJust $ runMaybeT do
    expectMissingPermission AllowTime [AllowRead, AllowWrite] "now!"
  where
    expectMissingPermission p ps str = MaybeT $ do
      catch
        (do execHiCmd str (Set.fromList ps)
            return Nothing)
        (\e -> case e of
            PermissionRequired p' | p == p -> return $ Just ()
            _ -> return Nothing)

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

evaluates_to :: String -> HiValue -> Bool
evaluates_to str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runFakeClock (eval e) of
        Left _  -> False
        Right v -> eqValue v expected

newtype FakeClock a = FakeClock (Identity a)
  deriving newtype (Functor, Applicative, Monad)

runFakeClock :: FakeClock a -> a
runFakeClock (FakeClock m) = runIdentity m

readTime :: String -> UTCTime
readTime = read

instance HiMonad FakeClock where
  runAction act = do
    case act of
      HiActionNow -> return (HiValueTime (readTime "2022-01-01 00:00:00 UTC"))
      _ -> error "FakeClock: unsupported action"

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
eqFn HiFunParseTime HiFunParseTime = True
eqFn _ _ = False

eqAction :: HiAction -> HiAction -> Bool
eqAction HiActionNow HiActionNow = True
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
eqValue (HiValueTime t1) (HiValueTime t2) = t1 == t2
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
