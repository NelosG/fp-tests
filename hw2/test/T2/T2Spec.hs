module T2Spec
  where

import Test.TAnnotated
import Test.TExcept
import Test.TList
import Test.TOption
import Test.TPair
import Test.TPrioritised
import Test.TQuad
import Test.Tasty (TestTree, testGroup)
import Test.TFun


tests :: IO TestTree
tests = do
  fun <- hspecFun
  return $ testGroup "HW2.T2" [propAnnotated, propExcept, propList, propOption, propPair, propPrioritised, propQuad, fun]
