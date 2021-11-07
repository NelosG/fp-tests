import T4Spec (tests)

import Hedgehog.Classes
import Test.State
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  test <- tests
  lawsCheck (functorLaws genState)
  lawsCheck (applicativeLaws genState)
  lawsCheck (monadLaws genState)
  defaultMain test
