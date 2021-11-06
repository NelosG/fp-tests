module T2Spec
  ( tests
  ) where

import Test.TAnnotated (propAnnotated)
import Test.TExcept (propExcept)
import Test.TFun (hspecFun, propFun)
import Test.TList (propList)
import Test.TOption (propOption)
import Test.TPair (propPair)
import Test.TPrioritised (propPrioritised)
import Test.TQuad (propQuad)
import Test.TStream (streamTests)
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  fun <- hspecFun
  stream <- streamTests
  return $ testGroup "HW2.T2" [propAnnotated, propExcept, propList, propOption, propPair, propPrioritised, propQuad, stream, fun, propFun]
