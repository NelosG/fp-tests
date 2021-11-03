import T6Spec (tests)

import Test.Tasty (defaultMain)

main :: IO ()
main = tests >>= \test -> defaultMain test
