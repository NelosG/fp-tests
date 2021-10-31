{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Test.T1Annotated where
import HW2.T1 (Annotated ((:#)), mapAnnotated)
import Hedgehog (Gen, Property, discover)
import Test.Common (compProp, genInt, idProp)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec

deriving instance ((Show a, Show e)) => Show (Annotated e a)
deriving instance ((Eq a, Eq e)) => Eq (Annotated e a)

hspecAnnotated :: IO TestTree
hspecAnnotated = testSpec "Annotated tests:" $ do
    describe "Annotated tests:" $ do
        it "Annotated test" $ mapAnnotated (+ 1) (1 :# "hell") `shouldBe` (2 :# "hell")
        it "Annotated(f . g) test" $ (mapAnnotated (+ 1) . mapAnnotated (* 10)) (1 :# "hell") `shouldBe` (11 :# "hell")

genAnnotated :: Gen (Annotated Int Int)
genAnnotated = (:#) <$> genInt <*> genInt

propAnnotated :: TestTree
propAnnotated = testGroup "Annotated properties" [
    testProperty "Annotated id property" $ idProp genAnnotated mapAnnotated
  , testProperty "Annotated composition property" $ compProp genAnnotated mapAnnotated
  ]
