module Test.T1Prioritised where
import HW2.T1 (Prioritised (High, Low, Medium), mapPrioritised)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance Eq a => Eq (Prioritised a) where
    (==) (Low a) (Low b)       = a == b
    (==) (Medium a) (Medium b) = a == b
    (==) (High a) (High b)     = a == b
    (==) _ _                   = False


hspecPrioritised :: IO TestTree
hspecPrioritised = testSpec "Prioritised tests:" $ do
    describe "Prioritised tests:" $ do
        it "Low test" $ mapPrioritised (+ 1) (Low 1) `shouldBe` Low 2
        it "Medium test" $ mapPrioritised (+ 1) (Medium 1) `shouldBe` Medium 2
        it "High test" $ mapPrioritised (+ 1) (High 1) `shouldBe` High 2

        it "Low(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (Low 1) `shouldBe` Low 11
        it "Medium(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (Medium 1) `shouldBe` Medium 11
        it "High(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (High 1) `shouldBe` High 11
