module Test.T1Option(hspecOption) where
import HW2.T1 (Option (Some, None), mapOption)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance Eq a => Eq (Option a) where
    (==) None None         = True
    (==) (Some a) (Some b) = a == b
    (==) _ _               = False


hspecOption :: IO TestTree
hspecOption = testSpec "Option tests:" $ do
    describe "None tests:" $ do
        it "None test" $ mapOption (+ 1) None `shouldBe` None
        it "None(f . g) test" $ (mapOption (+ 1) . mapOption (* 10)) None `shouldBe` None
    describe "Some tests:" $ do
        it "Some test" $ mapOption (+ 1) (Some 12) `shouldBe` Some 13
        it "Some(f . g) test" $ (mapOption (+ 1) . mapOption (* 10)) (Some 12) `shouldBe` Some 121
