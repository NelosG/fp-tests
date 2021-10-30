import T1Spec

import Test.Tasty

main :: IO ()
main = tests >>= \test -> defaultMain test