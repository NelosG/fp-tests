{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Annotated
  ( hspecAnnotated
  , propAnnotated
  ) where
import HW2.T1 (Annotated ((:#)), mapAnnotated)
import Hedgehog (Gen, Property, discover)
import Test.Common (allProps, compProp, genInt, idProp)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance ((Show a, Show e)) => Show (Annotated e a)
deriving instance ((Eq a, Eq e)) => Eq (Annotated e a)

hspecAnnotated :: IO TestTree
hspecAnnotated = testSpec "Annotated tests:" $ do
  it "Annotated test" $ mapAnnotated (+ 1) (1 :# "hell") `shouldBe` (2 :# "hell")
  it "Annotated(f . g) test" $ (mapAnnotated (+ 1) . mapAnnotated (* 10)) (1 :# "hell") `shouldBe` (11 :# "hell")

genAnnotated :: Gen (Annotated Int Int)
genAnnotated = (:#) <$> genInt <*> genInt

propAnnotated :: TestTree
propAnnotated = allProps "Annotated" genAnnotated mapAnnotated
