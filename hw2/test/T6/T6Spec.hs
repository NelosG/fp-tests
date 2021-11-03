module T6Spec where

import Test.Parser (propParser)
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  parser <- propParser
  return $ testGroup "HW2.T6" [ parser ]
