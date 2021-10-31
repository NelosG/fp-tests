module Test.T1Pair where
import HW2.T1 (Pair (P), mapPair)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance Eq a => Eq (Pair a) where
    (==) (P a b) (P c d) = a == c && b == d


hspecPair :: IO TestTree
hspecPair = testSpec "Pair tests:" $ do
    describe "Pair tests:" $ do
        it "Pair test" $ mapPair (+ 1) (P 1 2) `shouldBe` P 2 3
        it "Pair(f . g) test" $ (mapPair (+ 1) . mapPair (* 10)) (P 1 2) `shouldBe` P 11 21