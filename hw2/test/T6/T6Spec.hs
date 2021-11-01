module T6Spec where

import Test.Tasty
import Test.Parser

tests :: IO TestTree
tests = do
  parser <- propParser
  return $ testGroup "HW2.T6" [ parser ]
