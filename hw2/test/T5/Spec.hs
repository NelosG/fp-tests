import HW2.T5
import Hedgehog.Classes (applicativeLaws, functorLaws, lawsCheck, monadLaws)
import Main.Utf8 (withUtf8)
import T5Spec (tests)
import Test.ExceptState (genExceptState)
import Test.Tasty (defaultMain)

main :: IO ()
main = withUtf8 $ do
  test <- tests
  lawsCheck (functorLaws genExceptState)
  lawsCheck (applicativeLaws genExceptState)
  lawsCheck (monadLaws genExceptState)
  defaultMain test
