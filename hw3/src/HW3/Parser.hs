module HW3.Parser
  ( parse
  ) where

import Data.Void (Void)
import HW3.Base (HiExpr)
import Text.Megaparsec (ParseErrorBundle)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = undefined
