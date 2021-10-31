module Test.T1Annotated where
import HW2.T1 (Annotated ((:#)), mapAnnotated)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance (Eq a, Eq e) => Eq (Annotated a e) where
    (==) (a :# ae) (b :# be) = a == b && ae == be


hspecAnnotated :: IO TestTree
hspecAnnotated = testSpec "Annotated tests:" $ do
    describe "Annotated tests:" $ do
        it "Annotated test" $ mapAnnotated (+ 1) (1 :# "hell") `shouldBe` (2 :# "hell")
        it "Annotated(f . g) test" $ (mapAnnotated (+ 1) . mapAnnotated (* 10)) (1 :# "hell") `shouldBe` (11 :# "hell")