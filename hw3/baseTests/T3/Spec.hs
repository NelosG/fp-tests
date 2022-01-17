{-# LANGUAGE TemplateHaskell, NegativeLiterals #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import Data.Ratio ((%))
import Data.Fixed (Pico, showFixed)
import Numeric.Natural (Natural)
import Data.Void (Void)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Text.Megaparsec (ParseErrorBundle)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Functor.Classes (Eq1(liftEq))
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.String (renderString)
import qualified Language.Haskell.TH as TH

---------------------------
------ NAME CHECKING ------
---------------------------

import HW3.Base
  ( HiFun
    ( HiFunDiv
    , HiFunMul
    , HiFunAdd
    , HiFunSub
    , HiFunNot
    , HiFunAnd
    , HiFunOr
    , HiFunLessThan
    , HiFunGreaterThan
    , HiFunEquals
    , HiFunNotLessThan
    , HiFunNotGreaterThan
    , HiFunNotEquals
    , HiFunIf
    )
  )
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

hiValueBool' :: Bool -> HiValue
hiValueBool' = HiValueBool

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
    getTopPrec :: !Integer,
    getShrunk :: [Target]  -- N.B.: lazy field
  }

instance Show Target where
  show = getStr

instance Num Target where
  (*) = targetBinOp (HiFunMul, L, 7, "*")
  (+) = targetBinOp (HiFunAdd, L, 6, "+")
  (-) = targetBinOp (HiFunSub, L, 6, "-")
  fromInteger n =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational n)),
      getStr = show n,
      getShrunk = fmap fromInteger (QC.shrink n),
      getTopPrec = 10
    }
  abs = error "Target: abs"
  signum = error "Target: signum"

instance Fractional Target where
  (/) = targetBinOp (HiFunDiv, L, 7, "/")
  fromRational x =
    Target {
      getExpr = HiExprValue (HiValueNumber (toRational x')),
      getStr = showFixed True x',
      getShrunk = fmap fromRational (QC.shrink x),
      getTopPrec = 10
    }
    where x' = toPico x
  recip = error "Target: recip"

toPico :: Rational -> Pico
toPico = fromRational

fromPico :: Pico -> Rational
fromPico = toRational

data Fixity = L | N | R
  deriving (Eq)

targetBinOp :: (HiFun, Fixity, Integer, String) -> Target -> Target -> Target
targetBinOp fn@(fnSym, fnFix, fnPrec, fnName) lhs rhs =
    Target {
      getExpr = target,
      getStr = str,
      getTopPrec = fnPrec,
      getShrunk = shrunk
    }
  where
    target = HiExprApply (HiExprValue (HiValueFunction fnSym)) [getExpr lhs, getExpr rhs]
    str = wrap (fnFix /= L) lhs ++ fnName ++ wrap (fnFix /= R) rhs
    wrap glued targ
      | getTopPrec targ <= fnPrec && glued = "(" ++ getStr targ ++ ")"
      | getTopPrec targ <  fnPrec          = "(" ++ getStr targ ++ ")"
      | otherwise                          =        getStr targ
    shrunk =
      [lhs, rhs] ++
      [targetBinOp fn lhs' rhs' |
        (lhs', rhs') <- QC.shrink (lhs, rhs) ]

(.<), (.>), (.<=), (.>=), (.==), (./=) :: Target -> Target -> Target
(.<)  = targetBinOp (HiFunLessThan,       N, 4, "<")
(.>)  = targetBinOp (HiFunGreaterThan,    N, 4, ">")
(.<=) = targetBinOp (HiFunNotGreaterThan, N, 4, "<=")
(.>=) = targetBinOp (HiFunNotLessThan,    N, 4, ">=")
(.==) = targetBinOp (HiFunEquals,         N, 4, "==")
(./=) = targetBinOp (HiFunNotEquals,      N, 4, "/=")

(.&&) :: Target -> Target -> Target
(.&&) = targetBinOp (HiFunAnd, R, 3, "&&")
(.||) = targetBinOp (HiFunOr,  R, 2, "||")

genTarget :: Natural -> QC.Gen Target
genTarget 0 =
  QC.oneof
    [ fmap fromInteger QC.arbitrary,
      fmap (fromRational . fromPico) QC.arbitrary ]
genTarget n = do
  lhs <- genTarget (n `div` 2)
  rhs <- genTarget (n `div` 2)
  QC.oneof
    [ pure (lhs  +  rhs)
    , pure (lhs  -  rhs)
    , pure (lhs  *  rhs)
    , pure (lhs  /  rhs)
    , pure (lhs .<  rhs)
    , pure (lhs .>  rhs)
    , pure (lhs .<= rhs)
    , pure (lhs .>= rhs)
    , pure (lhs .== rhs)
    , pure (lhs ./= rhs)
    , pure (lhs .&& rhs)
    , pure (lhs .|| rhs)
    ]

instance QC.Arbitrary Target where
  arbitrary = genTarget 1000
  shrink = getShrunk

prop_parse_well_formed :: Target -> Bool
prop_parse_well_formed target =
  getStr target `parses_to` getExpr target

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "10 / 2" `parses_to` getExpr (10 / 2) &&
  "10 * 2" `parses_to` getExpr (10 * 2) &&
  "10 + 2" `parses_to` getExpr (10 + 2) &&
  "10 - 2" `parses_to` getExpr (10 - 2) &&
  "10 / 2.0" `parses_to` getExpr (10 / 2) &&
  "10 * 2.0" `parses_to` getExpr (10 * 2) &&
  "10 + 2.0" `parses_to` getExpr (10 + 2) &&
  "10 - 2.0" `parses_to` getExpr (10 - 2) &&
  "(10 + 15.1) / 3" `parses_to` getExpr ((10 + 15.1) / 3) &&
  "(2 * 5 + 1) / (11 - 6)" `parses_to` getExpr ((2 * 5 + 1) / (11 - 6)) &&
  "(mul(2, 5) + 1) / (11 - 6)" `parses_to` getExpr ((2 * 5 + 1) / (11 - 6)) &&
  does_not_parse "2 < 3 < 4"

prop_parse_whitespace_cases :: Bool
prop_parse_whitespace_cases =
  "( 10 + 15.1 ) / 3 " `parses_to` getExpr ((10 + 15.1) / 3)

prop_basic_session :: Bool
prop_basic_session =
  "2 + 2" `results_in_num` "4" &&
  "2 + 2 * 3" `results_in_num` "8" &&
  "(2 + 2) * 3" `results_in_num` "12" &&
  "2 + 2 * 3 == (2 + 2) * 3" `results_in_bool` "false" &&
  "10 == 2*5 && 143 == 11*13" `results_in_bool` "true"

prettyprints_num_to :: Rational -> String -> Bool
prettyprints_num_to n expected =
    ppr_num (HiValueNumber n) == expected
  where
    ppr_num = filter (not . isSpace) . renderString . layoutCompact . prettyValue

prettyprints_bool_to :: Bool -> String -> Bool
prettyprints_bool_to b expected =
    ppr_bool (HiValueBool b) == expected
  where
    ppr_bool = renderString . layoutCompact . prettyValue

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

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

does_not_parse :: String -> Bool
does_not_parse = isLeft . parse

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
eqFn _        _        = False

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
