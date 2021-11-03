{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Quad
  ( hspecQuad
  , propQuad
  ) where
import HW2.T1 (Quad (Q), mapQuad)
import Hedgehog (Gen)
import Test.Common (allProps, genInt)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a) => Show (Quad a)
deriving instance (Eq a) => Eq (Quad a)

hspecQuad :: IO TestTree
hspecQuad = testSpec "Quad tests:" $ do
  it "Quad test" $ mapQuad (+ 1) (Q 1 2 3 4) `shouldBe` Q 2 3 4 5
  it "Quad(f . g) test" $ (mapQuad (+ 1) . mapQuad (* 10)) (Q 1 2 3 4) `shouldBe` Q 11 21 31 41

genQuad :: Gen (Quad Int)
genQuad = Q <$> genInt <*> genInt <*> genInt <*> genInt

propQuad :: TestTree
propQuad = allProps "Quad" genQuad mapQuad
