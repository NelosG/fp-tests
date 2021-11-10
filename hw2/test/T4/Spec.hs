import T4Spec (tests)

import Hedgehog.Classes (applicativeLaws, functorLaws, lawsCheck, monadLaws)
import Main.Utf8 (withUtf8)
import Test.State (genState)
import Test.Tasty (defaultMain)

main :: IO ()
main = withUtf8 $ do
  test <- tests
  lawsCheck (functorLaws genState)
  lawsCheck (applicativeLaws genState)
  lawsCheck (monadLaws genState)
  defaultMain test
