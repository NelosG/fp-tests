import T4Spec (tests)

import Test.Tasty (defaultMain)

main :: IO ()
main = tests >>= \test -> defaultMain test
