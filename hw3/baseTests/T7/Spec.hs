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
    HiFunRead,
    HiFunWrite,
    HiFunMkDir,
    HiFunChDir
  ))
import HW3.Base (
  HiAction (
    HiActionRead,
    HiActionWrite,
    HiActionMkDir,
    HiActionChDir,
    HiActionCwd
  ))
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

hiActionRead' :: FilePath -> HiAction
hiActionRead' = HiActionRead

hiActionWrite' :: FilePath -> ByteString -> HiAction
hiActionWrite' = HiActionWrite

hiActionMkDir' :: FilePath -> HiAction
hiActionMkDir' = HiActionMkDir

hiActionChDir' :: FilePath -> HiAction
hiActionChDir' = HiActionChDir

hiActionCwd' :: HiAction
hiActionCwd' = HiActionCwd

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

hiFunRead' :: HiFun
hiFunRead' = HiFunRead

hiFunWrite' :: HiFun
hiFunWrite' = HiFunWrite

hiFunMkDir' :: HiFun
hiFunMkDir' = HiFunMkDir

hiFunChDir' :: HiFun
hiFunChDir' = HiFunChDir

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
  "read"  `parses_to` HiExprValue (HiValueFunction HiFunRead)  &&
  "write" `parses_to` HiExprValue (HiValueFunction HiFunWrite) &&
  "mkdir" `parses_to` HiExprValue (HiValueFunction HiFunMkDir) &&
  "cd"    `parses_to` HiExprValue (HiValueFunction HiFunChDir) &&
  "cwd"   `parses_to` HiExprValue (HiValueAction HiActionCwd)  &&
  "read(\"hello.txt\")!" `parses_to` HiExprRun (applyFn1 HiFunRead (HiExprValue (HiValueString (Text.pack "hello.txt")))) &&
  "mkdir(\"projects\")!" `parses_to` HiExprRun (applyFn1 HiFunMkDir (HiExprValue (HiValueString (Text.pack "projects")))) &&
  "cwd!"  `parses_to` HiExprRun (HiExprValue (HiValueAction HiActionCwd))

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "read(\"hi.txt\")"           `evaluates_to_act` HiActionRead  "hi.txt" &&
  "write(\"hi.txt\", \"Hi!\")" `evaluates_to_act` HiActionWrite "hi.txt" (BS.pack [0x48, 0x69, 0x21]) &&
  "mkdir(\"dir\")"             `evaluates_to_act` HiActionMkDir "dir" &&
  "cd(\"dir\")"                `evaluates_to_act` HiActionChDir "dir"

prop_eval_read_write :: Bool
prop_eval_read_write =
  case parse "write(\"Z\", read(\"X\")! + read(\"Y\")!)!" of
    Left _ -> False
    Right e ->
      case runStubFS (eval e) of
        (Left _, _) -> False
        (Right _, actlog) ->
          case actlog of
            [HiActionRead "X", HiActionRead "Y",  HiActionWrite "Z" expectedContents] -> True
            [HiActionRead "Y", HiActionRead "X",  HiActionWrite "Z" expectedContents] -> True
            _ -> False
  where
    expectedContents = Text.encodeUtf8 (Text.pack "[STUB:X][STUB:Y]")

prop_io_session :: QC.Property
prop_io_session =
  QC.ioProperty $ fmap isJust $ runMaybeT do
    MaybeT $ fmap Just $ removePathForcibly "tmpdir"
    exec "mkdir(\"tmpdir\")!"
    read_tmp_out <- exec "read(\"tmpdir\")!"
    guard $ read_tmp_out `eqValue` HiValueList Seq.empty
    exec "mkdir(\"tmpdir/a\")!"
    exec "mkdir(\"tmpdir/b\")!"
    read_tmp_out' <- exec "read(\"tmpdir\")!"
    guard $ read_tmp_out' `eqValue`
      HiValueList (Seq.fromList [
        HiValueString (Text.pack "a"),
        HiValueString (Text.pack "b")
      ])
    exec "write(\"tmpdir/hi.txt\", \"Hello\")!"
    exec "cd(\"tmpdir\")!"
    read_hi_out <- exec "read(\"hi.txt\")!"
    guard $ read_hi_out `eqValue`
      HiValueString (Text.pack "Hello")
    return ()
  where
    permissions = Set.fromList [AllowRead, AllowWrite]
    exec str = MaybeT (execHiCmd str permissions)

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
    expectMissingPermission AllowWrite [AllowRead] "write(\"tmpfile\", \"\")!"
    expectMissingPermission AllowWrite [AllowRead] "mkdir(\"tmpdir\")!"
    expectMissingPermission AllowRead [AllowWrite] "cwd!"
    expectMissingPermission AllowRead [AllowWrite] "cd(\"tmpdir\")!"
    expectMissingPermission AllowRead [AllowWrite] "read(\"tmpfile\")!"
  where
    expectMissingPermission p ps str = MaybeT $ do
      catch
        (do execHiCmd str (Set.fromList ps)
            return Nothing)
        (\e -> case e of
            PermissionRequired p' | p == p -> return $ Just ()
            _ -> return Nothing)

applyFn1 :: HiFun -> HiExpr -> HiExpr
applyFn1 fn arg = HiExprApply (HiExprValue (HiValueFunction fn)) [arg]

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
      case runStubFS (eval e) of
        (Left _, _)  -> False
        (Right v, _) -> eqValue v (HiValueAction expected)

newtype StubFS a = StubFS (State [HiAction] a)
  deriving newtype (Functor, Applicative, Monad)

runStubFS :: StubFS a -> (a, [HiAction])
runStubFS (StubFS m) =
  let (a, acts) = runState m [] in (a, reverse acts)

instance HiMonad StubFS where
  runAction act = do
    StubFS $ modify (act:)
    return $ case act of
      HiActionRead  path -> HiValueString (Text.pack ("[STUB:" ++ path ++ "]"))
      HiActionWrite _ _  -> HiValueNull
      HiActionMkDir _    -> HiValueNull
      HiActionChDir _    -> HiValueNull
      HiActionCwd        -> HiValueString (Text.pack "/stub")
      _                  -> error "StubFS: unsupported action"

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
eqFn HiFunRead HiFunRead = True
eqFn HiFunWrite HiFunWrite = True
eqFn HiFunMkDir HiFunMkDir = True
eqFn HiFunChDir HiFunChDir = True
eqFn _ _ = False

eqAction :: HiAction -> HiAction -> Bool
eqAction (HiActionRead p1) (HiActionRead p2) = p1 == p2
eqAction (HiActionWrite p1 c1) (HiActionWrite p2 c2) = p1 == p2 && c1 == c2
eqAction (HiActionMkDir p1) (HiActionMkDir p2) = p1 == p2
eqAction (HiActionChDir p1) (HiActionChDir p2) = p1 == p2
eqAction HiActionCwd HiActionCwd = True
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
