import T4Spec (tests)

import Hedgehog.Classes
import Test.State
import Test.Tasty (defaultMain)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do
  test <- tests
  lawsCheck (functorLaws genState)
  lawsCheck (applicativeLaws genState)
  lawsCheck (monadLaws genState)
  defaultMain test
