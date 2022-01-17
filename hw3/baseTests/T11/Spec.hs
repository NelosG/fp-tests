{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}

import Control.Monad (unless, (<=<))
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Text (Text)
import Numeric (showHex)
import Numeric.Natural (Natural)
import Data.Word (Word8)
import Data.Void (Void)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Text.Megaparsec (ParseErrorBundle)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Functor.Classes (Eq1(liftEq), Eq2(liftEq2))
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
    ----------------
    HiFunCount,
    HiFunKeys,
    HiFunValues,
    HiFunInvert
  ))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList, HiValueBytes, HiValueDict))
import HW3.Base (HiExpr(HiExprValue, HiExprApply, HiExprDict))
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

hiFunCount' :: HiFun
hiFunCount' = HiFunCount

hiFunKeys' :: HiFun
hiFunKeys' = HiFunKeys

hiFunValues' :: HiFun
hiFunValues' = HiFunValues

hiFunInvert' :: HiFun
hiFunInvert' = HiFunInvert

hiValueDict' :: Map HiValue HiValue -> HiValue
hiValueDict' = HiValueDict

hiExprDict' :: [(HiExpr, HiExpr)] -> HiExpr
hiExprDict' = HiExprDict

eval' :: HiExpr -> Identity (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "keys"   `parses_to` HiExprValue (HiValueFunction HiFunKeys) &&
  "values" `parses_to` HiExprValue (HiValueFunction HiFunValues) &&
  "count"  `parses_to` HiExprValue (HiValueFunction HiFunCount) &&
  "invert" `parses_to` HiExprValue (HiValueFunction HiFunInvert) &&
  "{ \"width\": 120, \"height\": 80 }" `parses_to`
    HiExprDict [
      (HiExprValue (HiValueString (Text.pack "width")), HiExprValue (HiValueNumber 120)),
      (HiExprValue (HiValueString (Text.pack "height")), HiExprValue (HiValueNumber 80))
    ] &&
  "{ 1: true, 3: true, 4: false }" `parses_to`
    HiExprDict [
      (HiExprValue (HiValueNumber 1), HiExprValue (HiValueBool True)),
      (HiExprValue (HiValueNumber 3), HiExprValue (HiValueBool True)),
      (HiExprValue (HiValueNumber 4), HiExprValue (HiValueBool False))
    ]

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "{ \"width\": 120, \"height\": 80 }(\"width\")" `evaluates_to_num` 120 &&
  "keys({ \"width\": 120, \"height\": 80 })" `evaluates_to_list_str` [Text.pack "height", Text.pack "width"] &&
  "values({ \"width\": 120, \"height\": 80 })" `evaluates_to_list_num` [80, 120] &&
  "count(\"XXXOX\")" `evaluates_to_dict`
    [(HiValueString (Text.pack "O"), HiValueNumber 1), (HiValueString (Text.pack "X"), HiValueNumber 4)]
    &&
  "count([# 58 58 58 4f 58 #])" `evaluates_to_dict`
    [(HiValueNumber 79, HiValueNumber 1), (HiValueNumber 88, HiValueNumber 4)]
    &&
  "count([true, true, false, true])" `evaluates_to_dict`
    [(HiValueBool False, HiValueNumber 1), (HiValueBool True, HiValueNumber 3)]
    &&
  (
    "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 })" `evaluates_to_dict`
      [(HiValueNumber 1, listOfStrings ["z", "x"])
      ,(HiValueNumber 2, listOfStrings ["y"])]
    ||
    "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 })" `evaluates_to_dict`
      [(HiValueNumber 1, listOfStrings ["x", "z"])
      ,(HiValueNumber 2, listOfStrings ["y"])]
  )

prop_parse_dot_access :: Bool
prop_parse_dot_access =
  "{}.hello.world" `parses_to` ((HiExprDict [] `dotAccess` "hello") `dotAccess` "world") &&
  "{}.hello(10).world" `parses_to` (HiExprApply (HiExprDict [] `dotAccess` "hello") [HiExprValue (HiValueNumber 10)] `dotAccess` "world") &&
  "({} + {}).hello" `parses_to` ((applyFn HiFunAdd [HiExprDict [], HiExprDict []]) `dotAccess` "hello")

dotAccess :: HiExpr -> String -> HiExpr
dotAccess e fld = HiExprApply e [HiExprValue (HiValueString (Text.pack fld))]

applyFn :: HiFun -> [HiExpr] -> HiExpr
applyFn fn args = HiExprApply (HiExprValue (HiValueFunction fn)) args

listOfStrings :: [String] -> HiValue
listOfStrings xs = (HiValueList . Seq.fromList) (map (HiValueString . Text.pack) xs)

prop_basic_session :: Bool
prop_basic_session =
  "count(\"Hello World\").o" `results_in_num` "2" &&
  "fold(add, values(count(\"Hello, World!\")))" `results_in_num` "13" &&
  "invert(count(\"big blue bag\"))(3) == [\"b\"]" `results_in_bool` "true"

prop_fcf_session :: Bool
prop_fcf_session =
  "{\"f\": add, \"g\": mul}.f" `results_in_fn` "add" &&
  "{\"f\": add, \"g\": mul}.g" `results_in_fn` "mul" &&
  "{\"f\": add, \"g\": mul}.f(10, 10)" `results_in_num` "20" &&
  "{\"f\": add, \"g\": mul}.g(10, 10)" `results_in_num` "100"

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

evaluates_to_list_str :: String -> [Text] -> Bool
evaluates_to_list_str str expected = str `evaluates_to_list` map HiValueString expected

evaluates_to_dict :: String -> [(HiValue, HiValue)] -> Bool
evaluates_to_dict str expected = str `evaluates_to` HiValueDict (Map.fromList expected)

prettyprints_fn_to :: HiFun -> String -> Bool
prettyprints_fn_to fn expected =
    ppr_fn (HiValueFunction fn) == expected
  where
    ppr_fn = renderString . layoutCompact . prettyValue

prettyprints_bool_to :: Bool -> String -> Bool
prettyprints_bool_to b expected =
    ppr_bool (HiValueBool b) == expected
  where
    ppr_bool = renderString . layoutCompact . prettyValue

prettyprints_num_to :: Rational -> String -> Bool
prettyprints_num_to n expected =
    ppr_num (HiValueNumber n) == expected
  where
    ppr_num = filter (not . isSpace) . renderString . layoutCompact . prettyValue

results_in_num :: String -> String -> Bool
results_in_num str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueNumber n) -> prettyprints_num_to n expected
        Right _ -> False

results_in_bool :: String -> String -> Bool
results_in_bool str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueBool b) -> prettyprints_bool_to b expected
        Right _ -> False

results_in_fn :: String -> String -> Bool
results_in_fn str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueFunction fn) -> prettyprints_fn_to fn expected
        Right _ -> False

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
eqFn HiFunCount HiFunCount = True
eqFn HiFunKeys HiFunKeys = True
eqFn HiFunValues HiFunValues = True
eqFn HiFunInvert HiFunInvert = True
eqFn _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
eqValue (HiValueString s1) (HiValueString s2) = s1 == s2
eqValue (HiValueList xs1) (HiValueList xs2) = liftEq eqValue xs1 xs2
eqValue (HiValueBytes bs1) (HiValueBytes bs2) = bs1 == bs2
eqValue HiValueNull HiValueNull = True
eqValue (HiValueDict d1) (HiValueDict d2) = liftEq2 eqValue eqValue d1 d2
eqValue _ _ = False

eqExpr :: HiExpr -> HiExpr -> Bool
eqExpr (HiExprApply fn1 args1) (HiExprApply fn2 args2) =
  eqExpr fn1 fn2 && liftEq eqExpr args1 args2
eqExpr (HiExprValue v1) (HiExprValue v2) =
  eqValue v1 v2
eqExpr (HiExprDict d1) (HiExprDict d2) = liftEq (liftEq2 eqExpr eqExpr) d1 d2
eqExpr _ _ = False

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
