{-# LANGUAGE DerivingStrategies #-}
module HW2.T6 where
import Control.Applicative
import Control.Monad
import qualified Data.Char
import HW2.T1
    
data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)


-- Here we use ExceptState for an entirely different purpose: to parse data from a string. 
-- Our state consists of a Natural representing how many characters we have already consumed (for error messages) and
--  the String is the remainder of the input.

runP :: Parser a -> String -> Except ParseError a

-- Let us define a parser that consumes a single character:

-- pChar :: Parser Char
-- pChar = P $ ES \(pos, s) ->
--   case s of
--     []     -> Error (ErrorAtPos pos)
--     (c:cs) -> Success (c :# (pos + 1, cs))
-- Study this definition:

-- What happens when the string is empty?
-- How does the parser state change when a character is consumed?
-- Write a comment that explains pChar.

-- Implement a parser that always fails:

parseError :: Parser a
-- Define the following instance:

instance Alternative Parser where
  empty = parseError
--   (<|>) = ...

instance MonadPlus Parser   -- No methods.
-- So that p <|> q tries to parse the input string using p, but in case of failure tries q.

-- Make sure that the laws hold:

-- empty <|> p  ≡  p
-- p <|> empty  ≡  p
-- Implement a parser that checks that there is no unconsumed input left (i.e. the string in the parser state is empty), 
-- and fails otherwise:

pEof :: Parser ()
-- Study the combinators provided by Control.Applicative and Control.Monad. The following are of particular interest:

-- msum
-- mfilter
-- optional
-- many
-- some
-- void
-- We can use them to construct more interesting parsers. 
-- For instance, here is a parser that accepts only non-empty sequences of uppercase letters:

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr
-- It can be used as follows:

-- ghci> runP pAbbr "HTML"
-- Success "HTML"

-- ghci> runP pAbbr "JavaScript"
-- Error (ErrorAtPos 1)
-- Using parser combinators, define the following function:

parseExpr :: String -> Except ParseError Expr
-- It must handle floating-point literals of the form 4.09, the operators + - * / with the usual precedence (multiplication and division bind tighter than addition and subtraction), and parentheses.

-- Example usage:

--   ghci> parseExpr "3.14 + 1.618 * 2"
--   Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))
--   ghci> parseExpr "2 * (1 + 3)"
--   Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))
--   ghci> parseExpr "24 + Hello"
--   Error (ErrorAtPos 3)