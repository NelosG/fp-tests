{-# LANGUAGE TemplateHaskell, NegativeLiterals #-}

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

import HW3.Base (HiFun(HiFunDiv, HiFunMul, HiFunAdd, HiFunSub))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction))
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

hiFunDiv' :: HiFun
hiFunDiv' = HiFunDiv

hiFunMul' :: HiFun
hiFunMul' = HiFunMul

hiFunAdd' :: HiFun
hiFunAdd' = HiFunAdd

hiFunSub' :: HiFun
hiFunSub' = HiFunSub

hiValueNumber' :: Rational -> HiValue
hiValueNumber' = HiValueNumber

hiValueFunction' :: HiFun -> HiValue
hiValueFunction' = HiValueFunction

hiExprValue' :: HiValue -> HiExpr
hiExprValue' = HiExprValue

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

data Target =
  Target {
    getExpr :: !HiExpr,
    getStr :: !String,
    getNum :: !(Either HiError Rational),
    getShrunk :: [Target]  -- N.B.: lazy field
  }

instance Show Target where
  show = getStr

instance Num Target where
  (*) = targetBinFn (HiFunMul, "mul", \a b -> Right (a*b))
  (+) = targetBinFn (HiFunAdd, "add", \a b -> Right (a+b))
  (-) = targetBinFn (HiFunSub, "sub", \a b -> Right (a-b))
  fromInteger n =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational n)),
      getStr = show n,
      getNum = Right (toRational n),
      getShrunk = fmap fromInteger (QC.shrink n)
    }
  abs = error "Target: abs"
  signum = error "Target: signum"

instance Fractional Target where
  (/) = targetBinFn (HiFunDiv, "div", \a b ->
    if b == 0 then Left HiErrorDivideByZero
              else Right (a / b))
  fromRational x =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational x')),
      getStr = showFixed True x',
      getNum = Right (toRational x'),
      getShrunk = fmap fromRational (QC.shrink x)
    }
    where x' = toPico x
  recip = error "Target: recip"

toPico :: Rational -> Pico
toPico = fromRational

fromPico :: Pico -> Rational
fromPico = toRational

targetBinFn :: (HiFun, String, Rational -> Rational -> Either HiError Rational) -> Target -> Target -> Target
targetBinFn fn@(fnSym, fnStr, fnEval) lhs rhs =
    Target {
      getExpr = target,
      getStr = str,
      getNum = num,
      getShrunk = shrunk
    }
  where
    target = HiExprApply (HiExprValue (HiValueFunction fnSym)) [getExpr lhs, getExpr rhs]
    str = fnStr ++ "(" ++ getStr lhs ++ ", " ++ getStr rhs ++ ")"
    num = do
      a <- getNum lhs
      b <- getNum rhs
      fnEval a b
    shrunk =
      [lhs, rhs] ++
      [targetBinFn fn lhs' rhs' |
        (lhs', rhs') <- QC.shrink (lhs, rhs) ]

genTarget :: Natural -> QC.Gen Target
genTarget 0 =
  QC.oneof
    [ fmap fromInteger QC.arbitrary,
      fmap (fromRational . fromPico) QC.arbitrary ]
genTarget n = do
  lhs <- genTarget (n `div` 2)
  rhs <- genTarget (n `div` 2)
  QC.oneof
    [ pure (lhs + rhs),
      pure (lhs - rhs),
      pure (lhs * rhs),
      pure (lhs / rhs) ]

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
  fmap HiValueNumber (getNum target)

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "div"    `parses_to` HiExprValue (HiValueFunction HiFunDiv) &&
  "mul"    `parses_to` HiExprValue (HiValueFunction HiFunMul) &&
  "add"    `parses_to` HiExprValue (HiValueFunction HiFunAdd) &&
  "sub"    `parses_to` HiExprValue (HiValueFunction HiFunSub) &&
  "2"      `parses_to` getExpr 2      &&
  "3.14"   `parses_to` getExpr 3.14   &&
  "-1.618" `parses_to` getExpr -1.618 &&
  "1.2e5"  `parses_to` getExpr 1.2e5  &&
  "div(10, 2)" `parses_to` getExpr (10 / 2) &&
  "mul(10, 2)" `parses_to` getExpr (10 * 2) &&
  "add(10, 2)" `parses_to` getExpr (10 + 2) &&
  "sub(10, 2)" `parses_to` getExpr (10 - 2) &&
  "div(10, 2.0)" `parses_to` getExpr (10 / 2) &&
  "mul(10, 2.0)" `parses_to` getExpr (10 * 2) &&
  "add(10, 2.0)" `parses_to` getExpr (10 + 2) &&
  "sub(10, 2.0)" `parses_to` getExpr (10 - 2) &&
  "div(add(10, 15.1), 3)" `parses_to` getExpr ((10 + 15.1) / 3)

prop_parse_whitespace_cases :: Bool
prop_parse_whitespace_cases =
  "div ( add ( 10 , 15.1 ) , 3 )" `parses_to` getExpr ((10 + 15.1) / 3)

prop_parse_garbage_cases :: Bool
prop_parse_garbage_cases =
  "add(1, 2, 3, sub)" `parses_to`
    HiExprApply (HiExprValue (HiValueFunction HiFunAdd))
      [ getExpr 1, getExpr 2, getExpr 3,
        HiExprValue (HiValueFunction HiFunSub) ]
    &&
  "add(1,2)(3,4)" `parses_to`
    HiExprApply
      (HiExprApply (HiExprValue (HiValueFunction HiFunAdd))
        [ getExpr 1, getExpr 2 ])
      [ getExpr 3, getExpr 4 ]

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "mul(2, 10)"   `evaluates_to_num` 20    &&
  "sub(1000, 7)" `evaluates_to_num` 993   &&
  "div(3, 5)"    `evaluates_to_num` 0.6   &&
  "add(500, 12)" `evaluates_to_num` 512   &&
  "sub(10, 100)" `evaluates_to_num` -90   &&
  "mul(23, 768)" `evaluates_to_num` 17664 &&
  "div(57, 190)" `evaluates_to_num` 0.3   &&
  "div(add(mul(2, 5), 1), sub(11,6))" `evaluates_to_num` 2.2

prop_basic_error_cases :: Bool
prop_basic_error_cases =
  "sub(1)"            `evaluates_to_err` HiErrorArityMismatch   &&
  "sub(add)"          `evaluates_to_err` HiErrorArityMismatch   &&
  "sub(1, 2, 3)"      `evaluates_to_err` HiErrorArityMismatch   &&
  "div(1, 0)"         `evaluates_to_err` HiErrorDivideByZero    &&
  "div(1, sub(5, 5))" `evaluates_to_err` HiErrorDivideByZero    &&
  "15(2)"             `evaluates_to_err` HiErrorInvalidFunction &&
  "sub(10, add)"      `evaluates_to_err` HiErrorInvalidArgument

prop_basic_ppr_cases :: Bool
prop_basic_ppr_cases =
  42       `prettyprints_num_to` "42"      &&
  3.14     `prettyprints_num_to` "3.14"    &&
  -8.15    `prettyprints_num_to` "-8.15"   &&
  77.01    `prettyprints_num_to` "77.01"   &&
  (1%3)    `prettyprints_num_to` "1/3"     &&
  (-1%7)   `prettyprints_num_to` "-1/7"    &&
  (3%11)   `prettyprints_num_to` "3/11"    &&
  (16%3)   `prettyprints_num_to` "5+1/3"   &&
  (-71%7)  `prettyprints_num_to` "-10-1/7" &&
  (267%11) `prettyprints_num_to` "24+3/11"

prettyprints_num_to :: Rational -> String -> Bool
prettyprints_num_to n expected =
    ppr_num (HiValueNumber n) == expected
  where
    ppr_num = filter (not . isSpace) . renderString . layoutCompact . prettyValue

prop_basic_session :: Bool
prop_basic_session =
  "100" `results_in_num` "100" &&
  "-15" `results_in_num` "-15" &&
  "add(100, -15)" `results_in_num` "85" &&
  "add(3, div(14, 100))" `results_in_num` "3.14" &&
  "div(10, 3)" `results_in_num` "3+1/3" &&
  "sub(mul(201, 11), 0.33)" `results_in_num` "2210.67"

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
eqFn _        _        = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2

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
