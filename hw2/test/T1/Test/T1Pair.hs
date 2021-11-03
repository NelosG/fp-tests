{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Pair
  ( hspecPair
  , propPair
  ) where
import HW2.T1 (Pair (P), mapPair)
import Hedgehog (Gen)
import Test.Common (allProps, genInt)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a) => Show (Pair a)
deriving instance (Eq a) => Eq (Pair a)

hspecPair :: IO TestTree
hspecPair = testSpec "Pair tests:" $ do
  it "Pair test" $ mapPair (+ 1) (P 1 2) `shouldBe` P 2 3
  it "Pair(f . g) test" $ (mapPair (+ 1) . mapPair (* 10)) (P 1 2) `shouldBe` P 11 21

genPair :: Gen (Pair Int)
genPair = P <$> genInt <*> genInt

propPair :: TestTree
propPair = allProps "Pair" genPair mapPair
