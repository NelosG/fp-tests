module HW3.Evaluator
  ( eval
  ) where

import HW3.Base (HiError, HiExpr, HiValue)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = undefined
