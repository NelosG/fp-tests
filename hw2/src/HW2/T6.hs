{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( -- * Datatypes
    ParseError (..)
  , Parser (..)
    -- * functions
  , pChar
  , pEof
  , parseError
  , parseExpr
  , runP
  ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (MonadPlus, many, mfilter, msum, optional, some, void)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (Op, Val), Prim (Add, Div, Mul, Sub))
import HW2.T5 (ExceptState (ES, runES))

newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a

-- | What happens when the string is empty?
-- | How does the parser state change when a character is consumed?
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a

instance Alternative Parser where
  empty = parseError
  (<|>) = ...

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()

parseExpr :: String -> Except ParseError Expr
