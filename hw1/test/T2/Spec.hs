import T2Spec (tests)

import Test.Tasty (defaultMain, testGroup)

-- comment  this import and advancedTests if you want run vase tests
import T2SpecAdvanced (advancedTests)


main :: IO ()
main = defaultMain (testGroup "HW1.T2" [tests, advancedTests])
