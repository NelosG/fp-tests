module HW3.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data HiFun

  = HiFunDiv --p7 l
  | HiFunMul --p7 l
  | HiFunAdd --p6 l
  | HiFunSub --p6 l

  | HiFunNot
  | HiFunAnd --p3 r
  | HiFunOr --p2 r
  | HiFunLessThan --p4
  | HiFunGreaterThan --p4
  | HiFunEquals --p4
  | HiFunNotLessThan --p4
  | HiFunNotGreaterThan --p4
  | HiFunNotEquals --p4
  | HiFunIf

  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim

  deriving (Show, Eq, Ord)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun

  | HiValueBool Bool

  | HiValueNull
  | HiValueString Text

  deriving (Show, Eq, Ord)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show, Eq, Ord)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd


class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
