{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Void (Void)
import Data.Char (isSpace)
import Text.Megaparsec (ParseErrorBundle)
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
    -----------
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim
  ))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString))
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

hiValueNull' :: HiValue
hiValueNull' = HiValueNull

hiValueString' :: Text -> HiValue
hiValueString' = HiValueString

hiFunLength' :: HiFun
hiFunLength' = HiFunLength

hiFunToUpper' :: HiFun
hiFunToUpper' = HiFunToUpper

hiFunToLower' :: HiFun
hiFunToLower' = HiFunToLower

hiFunReverse' :: HiFun
hiFunReverse' = HiFunReverse

hiFunTrim' :: HiFun
hiFunTrim' = HiFunTrim

eval' :: HiExpr -> Identity (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "length"    `parses_to` HiExprValue (HiValueFunction HiFunLength)  &&
  "to-upper"  `parses_to` HiExprValue (HiValueFunction HiFunToUpper) &&
  "to-lower"  `parses_to` HiExprValue (HiValueFunction HiFunToLower) &&
  "reverse"   `parses_to` HiExprValue (HiValueFunction HiFunReverse) &&
  "trim"      `parses_to` HiExprValue (HiValueFunction HiFunTrim)    &&
  "null"      `parses_to` HiExprValue HiValueNull &&
  str_lit_parse_case "Hello"       &&
  str_lit_parse_case "Hello World" &&
  str_lit_parse_case "  w h i t e  s p a c e  " &&
  str_lit_parse_case "42"

prop_escaping_parse_cases :: Bool
prop_escaping_parse_cases =
  str_lit_parse_case "newline: header\nfooter" &&
  str_lit_parse_case "tabs: 10\t20\t30" &&
  str_lit_parse_case "double quote: \"" &&
  str_lit_parse_case "single quote: \'" &&
  str_lit_parse_case "slash: \\"

prop_parse_string_lit :: QC.UnicodeString -> Bool
prop_parse_string_lit (QC.UnicodeString str) = str_lit_parse_case str

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "length(\"Hello World\")"   `evaluates_to_num` 11 &&
  "to-upper(\"Hello World\")" `evaluates_to_str` Text.pack "HELLO WORLD" &&
  "to-lower(\"Hello World\")" `evaluates_to_str` Text.pack "hello world" &&
  "reverse(\"stressed\")"     `evaluates_to_str` Text.pack "desserts"    &&
  "trim(\"  Hello World  \")" `evaluates_to_str` Text.pack "Hello World" &&
  "\"Hello\" + \"World\""     `evaluates_to_str` Text.pack "HelloWorld"  &&
  "\"Cat\" * 5"               `evaluates_to_str` Text.pack "CatCatCatCatCat" &&
  "\"/home/user\" / \"hi\""   `evaluates_to_str` Text.pack "/home/user/hi" &&
  "\"Hello World\"(0)"        `evaluates_to_str` Text.pack "H" &&
  "\"Hello World\"(1)"        `evaluates_to_str` Text.pack "e" &&
  "\"Hello World\"(2)"        `evaluates_to_str` Text.pack "l" &&
  "\"Hello World\"(3)"        `evaluates_to_str` Text.pack "l" &&
  "\"Hello World\"(4)"        `evaluates_to_str` Text.pack "o" &&
  "\"Hello World\"(5)"        `evaluates_to_str` Text.pack " " &&
  "\"Hello World\"(6)"        `evaluates_to_str` Text.pack "W" &&
  "\"Hello World\"(7)"        `evaluates_to_str` Text.pack "o" &&
  "\"Hello World\"(8)"        `evaluates_to_str` Text.pack "r" &&
  "\"Hello World\"(9)"        `evaluates_to_str` Text.pack "l" &&
  "\"Hello World\"(10)"       `evaluates_to_str` Text.pack "d" &&
  "\"Hello World\"(11)"       `evaluates_to_null` () &&
  "\"Hello World\"(99)"       `evaluates_to_null` () &&
  "\"Hello World\"(-1)"       `evaluates_to_null` () &&
  "\"Hello World\"(0, 5)"     `evaluates_to_str` Text.pack "Hello" &&
  "\"Hello World\"(2, 4)"     `evaluates_to_str` Text.pack "ll"

prop_eval_fcf_str :: Bool
prop_eval_fcf_str =
  "if(true, add, \"Hello World\")(2, 4)" `evaluates_to_num` 6 &&
  "if(false, add, \"Hello World\")(2, 4)" `evaluates_to_str` Text.pack "ll"

prop_eval_str_index_in_bounds :: QC.UnicodeString -> Bool
prop_eval_str_index_in_bounds (QC.UnicodeString str) =
  and [
     mkExpr i `evaluates_to_str` Text.singleton c
    | (i, c) <- zip [0..] str
  ]
    where mkExpr i = str' ++ "(" ++ show i ++ ")"
          str' = show str

prop_eval_str_slice_in_bounds :: QC.UnicodeString -> Bool
prop_eval_str_slice_in_bounds (QC.UnicodeString str) =
  and [
     mkExpr i k `evaluates_to_str` slice i k
    | i <- [0 .. n], k <- [0 .. n-i]
  ]
    where mkExpr i k = str' ++ show (i, i + k)
          str' = show str
          n = length str
          slice i k = Text.pack ((take k . drop i) str)

prop_basic_session :: Bool
prop_basic_session =
  "to-upper(\"what a nice language\")(7, 11)" `results_in_str` show "NICE" &&
  "\"Hello\" == \"World\"" `results_in_bool` "false" &&
  "length(\"Hello\" + \"World\")" `results_in_num` "10" &&
  "length(\"hehe\" * 5) / 3" `results_in_num` "6+2/3"

str_lit_parse_case :: String -> Bool
str_lit_parse_case str =
  show str `parses_to` HiExprValue (HiValueString (Text.pack str))

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

evaluates_to_num :: String -> Rational -> Bool
evaluates_to_num str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueNumber expected)

evaluates_to_str :: String -> Text -> Bool
evaluates_to_str str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueString expected)

evaluates_to_null :: String -> () -> Bool
evaluates_to_null str _ =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v HiValueNull

prettyprints_str_to :: Text -> String -> Bool
prettyprints_str_to s expected =
    ppr_str (HiValueString s) == expected
  where
    ppr_str = renderString . layoutCompact . prettyValue

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

results_in_str :: String -> String -> Bool
results_in_str str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueString s) -> prettyprints_str_to s expected
        Right _ -> False

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
eqFn _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
eqValue (HiValueString s1) (HiValueString s2) = s1 == s2
eqValue HiValueNull HiValueNull = True
eqValue _ _ = False

eqExpr :: HiExpr -> HiExpr -> Bool
eqExpr (HiExprApply fn1 args1) (HiExprApply fn2 args2) =
  eqExpr fn1 fn2 && liftEq eqExpr args1 args2
eqExpr (HiExprValue v1) (HiExprValue v2) =
  eqValue v1 v2
eqExpr _ _ = False

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
