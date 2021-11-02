{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Quad(hspecQuad, propQuad) where
import HW2.T1 (Quad (Q), mapQuad)
import Hedgehog
import Test.Common
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

deriving instance (Show a) => Show (Quad a)
deriving instance (Eq a) => Eq (Quad a)

hspecQuad :: IO TestTree
hspecQuad = testSpec "Quad tests:" $ do
    it "Quad test" $ mapQuad (+ 1) (Q 1 2 3 4) `shouldBe` Q 2 3 4 5
    it "Quad(f . g) test" $ (mapQuad (+ 1) . mapQuad (* 10)) (Q 1 2 3 4) `shouldBe` Q 11 21 31 41

genQuad :: Gen (Quad Int)
genQuad = Q <$> genInt <*> genInt <*> genInt <*> genInt

propQuad :: TestTree
propQuad = testGroup "Quad properties" [
    testProperty "Quad id property" $ idProp genQuad mapQuad
  , testProperty "Quad composition property" $ compProp genQuad mapQuad
  ]
