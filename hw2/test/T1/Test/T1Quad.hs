module Test.T1Quad(hspecQuad) where
import HW2.T1 (Quad (Q), mapQuad)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance Eq a => Eq (Quad a) where
    (==) (Q a b c d) (Q e f g h) = a == e && b == f && c == g && d == h


hspecQuad :: IO TestTree
hspecQuad = testSpec "Quad tests:" $ do
    describe "Quad tests:" $ do
        it "Quad test" $ mapQuad (+ 1) (Q 1 2 3 4) `shouldBe` Q 2 3 4 5
        it "Quad(f . g) test" $ (mapQuad (+ 1) . mapQuad (* 10)) (Q 1 2 3 4) `shouldBe` Q 11 21 31 41