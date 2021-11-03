import T3andT4Spec (advancedTests, tests)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Providers ()
import System.Environment ()

main :: IO ()
main = defaultMain (testGroup "HW1.T3 and HW1.T4" [tests, advancedTests])
    
