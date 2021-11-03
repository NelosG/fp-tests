module Test.TFun.Data
  ( Function (..)
  , getF
  ) where

import HW2.T1 (Fun (..))

data Function a = Fn (a -> a) String

getF :: Fun a b -> a -> b
getF (F a) = a
