import T1Spec

import Test.Tasty ( defaultMain )

main :: IO ()
main = tests >>= \test -> defaultMain test