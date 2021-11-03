{-# LANGUAGE StandaloneDeriving #-}

module Test.TOption where

import HW2.T1 (Option (None, Some), mapOption)
import HW2.T2 (distOption, wrapOption)
import HW2.T3 (joinOption)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Common (allProps, genString)
import Test.Hspec (describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

deriving instance (Show a) => Show (Option a)
deriving instance (Eq a) => Eq (Option a)

propOption :: TestTree
propOption = allProps "Option" genOption mapOption wrapOption distOption joinOption

genOption :: Gen (Option String)
genOption = Gen.choice [genNone, genSome]
  where
    genNone = Gen.constant None
    genSome = Some <$> genString

hspecOption :: IO TestTree
hspecOption = testSpec "Option tests:" $ do
  describe "None tests:" $ do
    describe "None test:" $ do
      let m = None :: Option (Option (Option Int))
      it "Base test" $ joinOption (joinOption m) `shouldBe` None
      it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinOption (mapOption joinOption m) `shouldBe` joinOption (joinOption m)
      it "\"joinF      (wrapF m)  =  m\" test" $ joinOption (wrapOption m) `shouldBe` m
      it "\"joinF (mapF wrapF m)  =  m\" test" $ joinOption (mapOption wrapOption m) `shouldBe` m
    describe "(Some None) test:" $ do
      let m = Some None :: Option (Option (Option Int))
      it "Base test" $ joinOption (joinOption m) `shouldBe` None
      it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinOption (mapOption joinOption m) `shouldBe` joinOption (joinOption m)
      it "\"joinF      (wrapF m)  =  m\" test" $ joinOption (wrapOption m) `shouldBe` m
      it "\"joinF (mapF wrapF m)  =  m\" test" $ joinOption (mapOption wrapOption m) `shouldBe` m
    describe "(Some (Some None)) test:" $ do
      let m = Some $ Some None :: Option (Option (Option Int))
      it "Base test" $ joinOption (joinOption m) `shouldBe` None
      it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinOption (mapOption joinOption m) `shouldBe` joinOption (joinOption m)
      it "\"joinF      (wrapF m)  =  m\" test" $ joinOption (wrapOption m) `shouldBe` m
      it "\"joinF (mapF wrapF m)  =  m\" test" $ joinOption (mapOption wrapOption m) `shouldBe` m

  describe "(Some (Some (Some a))) tests:" $ do
    let m = Some $ Some $ Some 1
    it "Base test" $ joinOption (joinOption m) `shouldBe` Some 1
    it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinOption (mapOption joinOption m) `shouldBe` joinOption (joinOption m)
    it "\"joinF      (wrapF m)  =  m\" test" $ joinOption (wrapOption m) `shouldBe` m
    it "\"joinF (mapF wrapF m)  =  m\" test" $ joinOption (mapOption wrapOption m) `shouldBe` m
