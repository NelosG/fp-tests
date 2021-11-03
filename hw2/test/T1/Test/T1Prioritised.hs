{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Prioritised
  ( hspecPrioritised
  , propPrioritised
  ) where
import HW2.T1 (Prioritised (High, Low, Medium), mapPrioritised)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Common (allProps, genInt)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a) => Show (Prioritised a)
deriving instance (Eq a) => Eq (Prioritised a)

hspecPrioritised :: IO TestTree
hspecPrioritised = testSpec "Prioritised tests:" $ do
  it "Low test" $ mapPrioritised (+ 1) (Low 1) `shouldBe` Low 2
  it "Medium test" $ mapPrioritised (+ 1) (Medium 1) `shouldBe` Medium 2
  it "High test" $ mapPrioritised (+ 1) (High 1) `shouldBe` High 2

  it "Low(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (Low 1) `shouldBe` Low 11
  it "Medium(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (Medium 1) `shouldBe` Medium 11
  it "High(f . g) test" $ (mapPrioritised (+ 1) . mapPrioritised (* 10)) (High 1) `shouldBe` High 11

genPrioritised :: Gen (Prioritised Int)
genPrioritised = Gen.choice [genLow, genMedium, genHigh]
  where
    genLow = Low <$> genInt
    genMedium = Medium <$> genInt
    genHigh = High <$> genInt

propPrioritised :: TestTree
propPrioritised = allProps "Prioritised" genPrioritised mapPrioritised
