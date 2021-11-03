import T6Spec (tests)

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  test <- tests
  defaultMain test
