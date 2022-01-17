{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments,
             StandaloneKindSignatures, ConstraintKinds, GeneralizedNewtypeDeriving,
             DerivingStrategies, FlexibleInstances #-}

import Control.Monad (unless, guard)
import System.Exit (exitFailure)
import Text.Megaparsec (ParseErrorBundle)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Kind (Type, Constraint)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Classes (Eq1(liftEq))
import Control.Monad.Trans.State
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
    HiFunEcho
  ))
import HW3.Base (HiAction (HiActionEcho))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList, HiValueBytes, HiValueAction))
import HW3.Base (HiExpr(HiExprValue, HiExprApply, HiExprRun))
import HW3.Base (HiError(HiErrorInvalidArgument, HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorDivideByZero))
import HW3.Base (HiMonad(runAction))
import HW3.Action (HiPermission(AllowRead, AllowWrite))
import HW3.Action (PermissionException(PermissionRequired))
import HW3.Action (HIO(HIO, runHIO))
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

---------------------------
------ TYPE CHECKING ------
---------------------------

hiActionEcho' :: Text -> HiAction
hiActionEcho' = HiActionEcho

type HiMonad' :: (Type -> Type) -> Constraint
type HiMonad' = HiMonad

runAction' :: HiMonad m => HiAction -> m HiValue
runAction' = runAction

type HiPermission' :: Type
type HiPermission' = HiPermission

_AllowRead' :: HiPermission
_AllowRead' = AllowRead

_AllowWrite' :: HiPermission
_AllowWrite' = AllowWrite

type PermissionException' :: Type
type PermissionException' = PermissionException

_PermissionRequired' :: HiPermission -> PermissionException
_PermissionRequired' = PermissionRequired

type HIO' :: Type -> Type
type HIO' = HIO

_HIO' :: (Set HiPermission -> IO a) -> HIO a
_HIO' = HIO

runHIO' :: HIO a -> Set HiPermission -> IO a
runHIO' = runHIO

hiFunEcho' :: HiFun
hiFunEcho' = HiFunEcho

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
  "echo" `parses_to` HiExprValue (HiValueFunction HiFunEcho) &&
  "echo(\"message\")!" `parses_to` HiExprRun (applyFn1 HiFunEcho (HiExprValue (HiValueString (Text.pack "message"))))

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "echo(\"message\")"      `evaluates_to` (HiValueAction (HiActionEcho (Text.pack "message")), []) &&
  "echo(\"message\")!"     `evaluates_to` (HiValueNull, [Text.pack "message"]) &&
  "\"Hello\"(0)  || \"Z\"" `evaluates_to` (HiValueString (Text.pack "H"), []) &&
  "\"Hello\"(99) || \"Z\"" `evaluates_to` (HiValueString (Text.pack "Z"), []) &&
  "if(2 == 2, echo(\"OK\")!, echo(\"WTF\")!)" `evaluates_to` (HiValueNull, [Text.pack "OK"]) &&
  "true || echo(\"Don't do this\")!"  `evaluates_to` (HiValueBool True,  []) &&
  "false && echo(\"Don't do this\")!" `evaluates_to` (HiValueBool False, []) &&
  "[# 00 ff #] && echo(\"Just do it\")!" `evaluates_to` (HiValueNull, [Text.pack "Just do it"])

prop_eval_div_by_zero :: Bool
prop_eval_div_by_zero =
  "if(true,  1, 0/0)" `evaluates_to`     (HiValueNumber 1, []) &&
  "if(false, 1, 0/0)" `evaluates_to_err` HiErrorDivideByZero

applyFn1 :: HiFun -> HiExpr -> HiExpr
applyFn1 fn arg = HiExprApply (HiExprValue (HiValueFunction fn)) [arg]

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

newtype EchoLog a = EchoLog (State [Text] a)
  deriving newtype (Functor, Applicative, Monad)

runEchoLog :: EchoLog a -> (a, [Text])
runEchoLog (EchoLog m) =
  let (a, msgs) = runState m [] in (a, reverse msgs)

instance HiMonad EchoLog where
  runAction act =
    EchoLog $ case act of
      HiActionEcho msg -> do
        modify (msg:)
        return HiValueNull
      _ -> error "EchoLog: unsupported action"

evaluates_to :: String -> (HiValue, [Text]) -> Bool
evaluates_to str (expected_v, expected_msglog) =
  case parse str of
    Left _ -> False
    Right e ->
      case runEchoLog (eval e) of
        (Left _, _) -> False
        (Right v, msglog) ->
          eqValue v expected_v &&
          msglog == expected_msglog

evaluates_to_err :: String -> HiError -> Bool
evaluates_to_err str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runEchoLog (eval e) of
        (Right _, _)  -> False
        (Left err, _) -> eqError err expected

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
eqFn HiFunEcho HiFunEcho = True
eqFn _ _ = False

eqAction :: HiAction -> HiAction -> Bool
eqAction (HiActionEcho msg1) (HiActionEcho msg2) = msg1 == msg2
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
