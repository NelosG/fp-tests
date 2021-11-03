module T3Spec
  ( tests
  ) where


import Test.TAnnotated (hspecAnnotated, propAnnotated)
import Test.TExcept (hspecExcept, propExcept)
import Test.TFun (hspecFun)
import Test.TList (hspecList, propList)
import Test.TOption (hspecOption, propOption)
import Test.Tasty (TestTree, testGroup)


tests :: IO TestTree
tests = do
  option <- hspecOption
  annotated <- hspecAnnotated
  except <- hspecExcept
  list <- hspecList
  fun <- hspecFun
  return $ testGroup "HW2.T3" [option, annotated, except, list, fun, propAnnotated, propExcept, propList, propOption]
