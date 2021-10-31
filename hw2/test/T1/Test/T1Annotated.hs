{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Test.T1Annotated where
import HW2.T1 (Annotated ((:#)), mapAnnotated)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Common (genInt, idProp, compProp)
import Hedgehog (Gen, Property, discover)
import Test.Tasty.Hedgehog (testProperty)

deriving instance _ => Show (Annotated e a)
deriving instance _ => Eq (Annotated e a)

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
