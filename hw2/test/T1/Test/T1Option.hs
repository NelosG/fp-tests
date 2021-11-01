{-# LANGUAGE StandaloneDeriving #-}

module Test.T1Option(hspecOption, propOption) where
import HW2.T1 (Option (None, Some), mapOption)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Common
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

deriving instance (Show a) => Show (Option a)
deriving instance (Eq a) => Eq (Option a)


hspecOption :: IO TestTree
hspecOption = testSpec "Option tests:" $ do
    describe "None tests:" $ do
        it "None test" $ mapOption (+ 1) None `shouldBe` None
        it "None(f . g) test" $ (mapOption (+ 1) . mapOption (* 10)) None `shouldBe` None
    describe "Some tests:" $ do
        it "Some test" $ mapOption (+ 1) (Some 12) `shouldBe` Some 13
        it "Some(f . g) test" $ (mapOption (+ 1) . mapOption (* 10)) (Some 12) `shouldBe` Some 121

genOption :: Gen (Option Int)
genOption = Gen.choice [genNone, genSome]
  where
    genNone = Gen.constant None
    genSome = Some <$> genInt

propOption :: TestTree
propOption = testGroup "Option properties" [
    testProperty "Option id property" $ idProp genOption mapOption
  , testProperty "Option composition property" $ compProp genOption mapOption
  ]
