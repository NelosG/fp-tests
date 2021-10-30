import T5Spec

import Test.Tasty

main :: IO ()
main = tests >>= \test -> defaultMain test