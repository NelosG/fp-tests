{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import Data.Ratio ((%))
import Data.Fixed (Pico, showFixed)
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
    ---------
    HiFunNot,
    HiFunAnd,
    HiFunOr,
    HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunIf
  ))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool))
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

hiValueBool' :: Bool -> HiValue
hiValueBool' = HiValueBool

hiFunNot' :: HiFun
hiFunNot' = HiFunNot

hiFunAnd' :: HiFun
hiFunAnd' = HiFunAnd

hiFunOr' :: HiFun
hiFunOr' = HiFunOr

hiFunLessThan' :: HiFun
hiFunLessThan' = HiFunLessThan

hiFunGreaterThan' :: HiFun
hiFunGreaterThan' = HiFunGreaterThan

hiFunEquals' :: HiFun
hiFunEquals' = HiFunEquals

hiFunNotLessThan' :: HiFun
hiFunNotLessThan' = HiFunNotLessThan

hiFunNotGreaterThan' :: HiFun
hiFunNotGreaterThan' = HiFunNotGreaterThan

hiFunNotEquals' :: HiFun
hiFunNotEquals' = HiFunNotEquals

hiFunIf' :: HiFun
hiFunIf' = HiFunIf

eval' :: HiExpr -> Identity (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

data Target =
  Target {
    getExpr :: !HiExpr,
    getStr :: !String,
    getValue :: !HiValue,
    getShrunk :: [Target]  -- N.B.: lazy field
  }

instance Show Target where
  show = getStr

instance Num Target where
  (*) = targetBinNumFn HiFunMul "mul" (*)
  (+) = targetBinNumFn HiFunAdd "add" (+)
  (-) = targetBinNumFn HiFunSub "sub" (-)
  fromInteger n =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational n)),
      getStr = show n,
      getValue = HiValueNumber (toRational n),
      getShrunk = fmap fromInteger (QC.shrink n)
    }
  abs = error "test suite bug: Target: abs"
  signum = error "test suite bug: Target: signum"

instance Fractional Target where
  (/) = targetBinNumFn HiFunDiv "div" \a b ->
    if b == 0 then error "test suite bug: divide by zero"
              else a / b
  fromRational x =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational x')),
      getStr = showFixed True x',
      getValue = HiValueNumber (toRational x'),
      getShrunk = fmap fromRational (QC.shrink x)
    }
    where x' = toPico x
  recip = error "test suite bug: Target: recip"

_True :: Target
_True =
  Target {
    getExpr = HiExprValue (HiValueBool True),
    getStr = "true",
    getValue = HiValueBool True,
    getShrunk = []
  }

_False :: Target
_False =
  Target {
    getExpr = HiExprValue (HiValueBool False),
    getStr = "false",
    getValue = HiValueBool False,
    getShrunk = []
  }

(.&&), (.||) :: Target -> Target -> Target
(.&&) = targetBinBoolFn HiFunAnd "and" (&&)
(.||) = targetBinBoolFn HiFunOr  "or"  (||)

(.<), (.>), (.>=), (.<=), (.==), (./=) :: ShrinkToOperands -> Target -> Target -> Target
(.<) sto = targetBinCmpFn sto HiFunLessThan    "less-than"    \a b -> cmpValue a b == LT
(.>) sto = targetBinCmpFn sto HiFunGreaterThan "greater-than" \a b -> cmpValue a b == GT
(.>=) sto = targetBinCmpFn sto HiFunNotLessThan    "not-less-than"    \a b -> cmpValue a b /= LT
(.<=) sto = targetBinCmpFn sto HiFunNotGreaterThan "not-greater-than" \a b -> cmpValue a b /= GT
(.==) sto = targetBinFn sto HiFunEquals    "equals"     \a b -> HiValueBool (eqValue a b)
(./=) sto = targetBinFn sto HiFunNotEquals "not-equals" \a b -> HiValueBool (not (eqValue a b))

_not :: Target -> Target
_not e =
  Target {
    getExpr = HiExprApply (HiExprValue (HiValueFunction HiFunNot)) [getExpr e],
    getStr = "not(" ++ getStr e ++ ")",
    getValue =
      case getValue e of
        HiValueBool b -> HiValueBool (not b)
        _ -> error "test suite bug: not: expected a boolean",
    getShrunk = [e] ++ [_not e' | e' <- QC.shrink e]
  }

_if :: Target -> Target -> Target -> Target
_if cond _then _else =
  Target {
    getExpr = target,
    getStr = str,
    getValue = value,
    getShrunk = shrunk
  }
  where
    target = HiExprApply (HiExprValue (HiValueFunction HiFunIf))
      [getExpr cond, getExpr _then, getExpr _else]
    str = "if(" ++ getStr cond ++ ", " ++ getStr _then ++ ", " ++ getStr _else ++ ")"
    value =
      case getValue cond of
        HiValueBool b -> if b then getValue _then else getValue _else
        _ -> error "test suite bug: if: expected a boolean"
    shrunk =
      -- cannot shrink to cond: not type-preserving
      [_then, _else] ++
      [_if cond' _then' _else' |
        (cond', _then', _else') <- QC.shrink (cond, _then, _else) ]

toPico :: Rational -> Pico
toPico = fromRational

fromPico :: Pico -> Rational
fromPico = toRational

targetBinNumFn :: HiFun -> String -> (Rational -> Rational -> Rational) ->
    Target -> Target -> Target
targetBinNumFn fnSym fnStr fnEval =
  targetBinFn (ShrinkToOperands True) fnSym fnStr \a b ->
    case (a, b) of
      (HiValueNumber n, HiValueNumber m) -> HiValueNumber (fnEval n m)
      _ -> error "test suite bug: num fn: not a numeric argument"

targetBinBoolFn :: HiFun -> String -> (Bool -> Bool -> Bool) ->
  Target -> Target -> Target
targetBinBoolFn fnSym fnStr fnEval =
  targetBinFn (ShrinkToOperands True) fnSym fnStr \a b ->
    case (a, b) of
      (HiValueBool n, HiValueBool m) -> HiValueBool (fnEval n m)
      _ -> error "test suite bug: bool fn: not a boolean argument"

targetBinCmpFn :: ShrinkToOperands -> HiFun -> String -> (HiValue -> HiValue -> Bool) ->
  Target -> Target -> Target
targetBinCmpFn sto fnSym fnStr fnEval =
  targetBinFn sto fnSym fnStr \a b -> HiValueBool (fnEval a b)

newtype ShrinkToOperands = ShrinkToOperands Bool

targetBinFn :: ShrinkToOperands -> HiFun -> String -> (HiValue -> HiValue -> HiValue) ->
  Target -> Target -> Target
targetBinFn sto@(ShrinkToOperands sto') fnSym fnStr fnEval lhs rhs =
    Target {
      getExpr = target,
      getStr = str,
      getValue = value,
      getShrunk = shrunk
    }
  where
    target = HiExprApply (HiExprValue (HiValueFunction fnSym)) [getExpr lhs, getExpr rhs]
    str = fnStr ++ "(" ++ getStr lhs ++ ", " ++ getStr rhs ++ ")"
    value = fnEval (getValue lhs) (getValue rhs)
    shrunk =
      (if sto' then [lhs, rhs] else []) ++
      [targetBinFn sto fnSym fnStr fnEval lhs' rhs' |
        (lhs', rhs') <- QC.shrink (lhs, rhs) ]

genTarget :: Natural -> QC.Gen Target
genTarget n = QC.oneof [genNumTarget n, genBoolTarget n]

genNumTarget :: Natural -> QC.Gen Target
genNumTarget 0 =
  QC.oneof
    [ fmap fromInteger QC.arbitrary,
      fmap (fromRational . fromPico) QC.arbitrary ]
genNumTarget n =
  QC.oneof [
    genArithExpr n,
    genIfExpr genNumTarget n
  ]

genIfExpr :: (Natural -> QC.Gen Target) -> (Natural -> QC.Gen Target)
genIfExpr genBranch n = do
  cond <- genBoolTarget (n `div` 3)
  lhs <- genBranch (n `div` 3)
  rhs <- genBranch (n `div` 3)
  return (_if cond lhs rhs)

genBoolTarget :: Natural -> QC.Gen Target
genBoolTarget 0 = do
  b <- QC.arbitrary
  pure (if b then _True else _False)
genBoolTarget n =
  QC.oneof [
    genCmpExpr n,
    genLogicExpr n,
    genIfExpr genBoolTarget n
  ]

genCmpExpr :: Natural -> QC.Gen Target
genCmpExpr n = do
  (sto, genOrdTarget) <- QC.elements
    [ (ShrinkToOperands True, genBoolTarget),
      (ShrinkToOperands False, genNumTarget) ]
  lhs <- genOrdTarget (n `div` 2)
  rhs <- genOrdTarget (n `div` 2)
  QC.elements
    [ (.<) sto lhs rhs,
      (.>) sto lhs rhs,
      (.<=) sto lhs rhs,
      (.>=) sto lhs rhs,
      (.==) sto lhs rhs,
      (./=) sto lhs rhs ]

genArithExpr :: Natural -> QC.Gen Target
genArithExpr n = do
  lhs <- genNumTarget (n `div` 2)
  rhs <- genNumTarget (n `div` 2)
  QC.elements
    ([ lhs + rhs,
       lhs - rhs,
       lhs * rhs
     ] ++
     [ lhs / rhs | nonZero (getValue rhs) ])
  where
    nonZero (HiValueNumber x) = x /= 0
    nonZero _ = error "test suite bug: arith expr: not a numeric argument"

genLogicExpr :: Natural -> QC.Gen Target
genLogicExpr n =
  QC.oneof
    [ pure (.&&) <*> genBoolTarget (n `div` 2) <*> genBoolTarget (n `div` 2),
      pure (.||) <*> genBoolTarget (n `div` 2) <*> genBoolTarget (n `div` 2),
      fmap _not (genBoolTarget (n - 1)) ]

instance QC.Arbitrary Target where
  arbitrary = genTarget 1000
  shrink = getShrunk

prop_parse_well_formed :: Target -> Bool
prop_parse_well_formed target =
  getStr target `parses_to` getExpr target

prop_eval_well_formed :: Target -> Bool
prop_eval_well_formed target =
  runIdentity (eval (getExpr target))
    `eqResult`
  Right (getValue target)

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "not"                 `parses_to` HiExprValue (HiValueFunction HiFunNot) &&
  "and"                 `parses_to` HiExprValue (HiValueFunction HiFunAnd) &&
  "or"                  `parses_to` HiExprValue (HiValueFunction HiFunOr)  &&
  "less-than"           `parses_to` HiExprValue (HiValueFunction HiFunLessThan)    &&
  "greater-than"        `parses_to` HiExprValue (HiValueFunction HiFunGreaterThan) &&
  "equals"              `parses_to` HiExprValue (HiValueFunction HiFunEquals)      &&
  "not-less-than"       `parses_to` HiExprValue (HiValueFunction HiFunNotLessThan) &&
  "not-greater-than"    `parses_to` HiExprValue (HiValueFunction HiFunNotGreaterThan) &&
  "not-equals"          `parses_to` HiExprValue (HiValueFunction HiFunNotEquals) &&
  "if"                  `parses_to` HiExprValue (HiValueFunction HiFunIf) &&
  "true"                `parses_to` HiExprValue (HiValueBool True)  &&
  "false"               `parses_to` HiExprValue (HiValueBool False)

prop_parse_fun_app_chain :: Bool
prop_parse_fun_app_chain =
  "if(false, add, mul)(10, 10)" `parses_to`
      HiExprApply
        (HiExprApply (HiExprValue (HiValueFunction HiFunIf))
          [ HiExprValue (HiValueBool False),
            HiExprValue (HiValueFunction HiFunAdd),
            HiExprValue (HiValueFunction HiFunMul) ])
        [ HiExprValue (HiValueNumber 10),
          HiExprValue (HiValueNumber 10) ]

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "not(true)"  `evaluates_to_bool` False &&
  "not(false)" `evaluates_to_bool` True  &&
  "and(true, true)"   `evaluates_to_bool` True  &&
  "and(true, false)"  `evaluates_to_bool` False &&
  "and(false, true)"  `evaluates_to_bool` False &&
  "and(false, false)" `evaluates_to_bool` False &&
  "or(true, true)"    `evaluates_to_bool` True  &&
  "or(true, false)"   `evaluates_to_bool` True  &&
  "or(false, true)"   `evaluates_to_bool` True  &&
  "or(false, false)"  `evaluates_to_bool` False &&
  "equals(10, 10)"       `evaluates_to_bool` True  &&
  "equals(false, false)" `evaluates_to_bool` True  &&
  "equals(3, 10)"        `evaluates_to_bool` False &&
  "equals(1, true)"      `evaluates_to_bool` False &&
  "less-than(3, 3)"        `evaluates_to_bool` False &&
  "less-than(3, 10)"       `evaluates_to_bool` True  &&
  "less-than(10, 3)"       `evaluates_to_bool` False &&
  "less-than(false, true)" `evaluates_to_bool` True  &&
  "less-than(true, false)" `evaluates_to_bool` False &&
  "less-than(false, 0)"    `evaluates_to_bool` True  &&
  "if(true, false, true)"  `evaluates_to_bool` False &&
  "if(false, false, true)" `evaluates_to_bool` True

prop_eval_complements :: Target -> Target -> Bool
prop_eval_complements a b =
    (.<)  sto a b `equal_results` (.>) sto b a &&
    (./=) sto a b `equal_results` _not ((.==) sto a b) &&
    (.>=) sto a b `equal_results` _not ((.<) sto a b) &&
    (.<=) sto a b `equal_results` _not ((.>) sto a b)
  where
    sto = ShrinkToOperands False

prop_eval_branching :: Target -> Target -> Bool
prop_eval_branching a b =
  _if _True  a b `equal_results` a &&
  _if _False a b `equal_results` b

prop_basic_error_cases :: Bool
prop_basic_error_cases =
  "if(true, 0, 0, 0)" `evaluates_to_err` HiErrorArityMismatch &&
  "if(true, 0)"       `evaluates_to_err` HiErrorArityMismatch &&
  "if(0, 0, 0)"       `evaluates_to_err` HiErrorInvalidArgument &&
  "not(0)"            `evaluates_to_err` HiErrorInvalidArgument &&
  "true(0)"           `evaluates_to_err` HiErrorInvalidFunction

prop_basic_ppr_cases :: Bool
prop_basic_ppr_cases =
  True  `prettyprints_bool_to` "true" &&
  False `prettyprints_bool_to` "false"

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

prop_basic_session :: Bool
prop_basic_session =
  "false" `results_in_bool` "false" &&
  "equals(add(2, 2), 4)" `results_in_bool` "true" &&
  "less-than(mul(999, 99), 10000)" `results_in_bool` "false" &&
  "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" `results_in_num` "-1" &&
  "and(less-than(0, 1), less-than(1, 0))" `results_in_bool` "false" &&
  "or(less-than(0, 1), less-than(1, 0))"  `results_in_bool` "true"

-- first-class functions
prop_fcf_session :: Bool
prop_fcf_session =
  "if(true, add, mul)"  `results_in_fn` "add" &&
  "if(false, add, mul)" `results_in_fn` "mul" &&
  "if(true, add, mul)(10, 10)"  `results_in_num` "20" &&
  "if(false, add, mul)(10, 10)" `results_in_num` "100" &&
  "equals(add, add)" `results_in_bool` "true" &&
  "equals(add, mul)" `results_in_bool` "false"

-- QuickCheck wrapper
newtype QCW a = QCW a
  deriving newtype (Eq, Show)

instance QC.Arbitrary (QCW HiFun) where
  arbitrary = fmap QCW $
    QC.elements [
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
      HiFunIf
    ]

prop_eval_fcf_equals :: QCW HiFun -> QCW HiFun -> Bool
prop_eval_fcf_equals (QCW fn1) (QCW fn2) =
    runIdentity (eval e)
      `eqResult`
    Right (HiValueBool (eqFn fn1 fn2))
  where
    e = HiExprApply (HiExprValue (HiValueFunction HiFunEquals))
          [ HiExprValue (HiValueFunction fn1),
            HiExprValue (HiValueFunction fn2) ]

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

evaluates_to_bool :: String -> Bool -> Bool
evaluates_to_bool str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueBool expected)

evaluates_to_err :: String -> HiError -> Bool
evaluates_to_err str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Right _ -> False
        Left err -> eqError err expected

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

equal_results :: Target -> Target -> Bool
equal_results e1 e2 =
  runIdentity (eval (getExpr e1))
   `eqResult`
  runIdentity (eval (getExpr e2))

cmpValue :: HiValue -> HiValue -> Ordering
cmpValue (HiValueBool _) (HiValueNumber _) = LT
cmpValue (HiValueNumber _) (HiValueBool _) = GT
cmpValue (HiValueBool b1) (HiValueBool b2) = compare b1 b2
cmpValue (HiValueNumber x1) (HiValueNumber x2) = compare x1 x2
cmpValue _ _ = error "test suite bug: cmpValue: invalid input"

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
eqFn _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
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
