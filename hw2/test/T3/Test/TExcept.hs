{-# LANGUAGE StandaloneDeriving #-}

module Test.TExcept
  where

import HW2.T1 (Except (Error, Success), mapExcept)
import HW2.T2
import HW2.T3
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Common
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

deriving instance (Show a, Show e) => Show (Except e a)
deriving instance (Eq a, Eq e) => Eq (Except e a)


hspecExcept :: IO TestTree
hspecExcept = testSpec "Except tests:" $ do
    describe "Error tests:" $ do
        describe "Error test:" $ do
            let m = Error "hello" :: Except String (Except String (Except String Int))
            it "Base test" $ joinExcept (joinExcept m) `shouldBe` Error "hello"
            it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinExcept (mapExcept joinExcept m) `shouldBe` joinExcept (joinExcept m)
            it "\"joinF      (wrapF m)  =  m\" test" $ joinExcept (wrapExcept m) `shouldBe` m
            it "\"joinF (mapF wrapF m)  =  m\" test" $ joinExcept (mapExcept wrapExcept m) `shouldBe` m
        describe "(Success Error) test:" $ do
            let m = Success $ Error "hello" :: Except String (Except String (Except String Int))
            it "Base test" $ joinExcept (joinExcept m) `shouldBe` Error "hello"
            it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinExcept (mapExcept joinExcept m) `shouldBe` joinExcept (joinExcept m)
            it "\"joinF      (wrapF m)  =  m\" test" $ joinExcept (wrapExcept m) `shouldBe` m
            it "\"joinF (mapF wrapF m)  =  m\" test" $ joinExcept (mapExcept wrapExcept m) `shouldBe` m
        describe "(Success (Success Error)) test:" $ do
            let m = Success $ Success $ Error "hello" :: Except String (Except String (Except String Int))
            it "Base test" $ joinExcept (joinExcept m) `shouldBe` Error "hello"
            it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinExcept (mapExcept joinExcept m) `shouldBe` joinExcept (joinExcept m)
            it "\"joinF      (wrapF m)  =  m\" test" $ joinExcept (wrapExcept m) `shouldBe` m
            it "\"joinF (mapF wrapF m)  =  m\" test" $ joinExcept (mapExcept wrapExcept m) `shouldBe` m

    describe "(Success (Success (Success a))) test:" $ do
        let m = Success $ Success $ Success "hello" :: Except String (Except String (Except String String))
        it "Base test" $ joinExcept (joinExcept m) `shouldBe` Success "hello"
        it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinExcept (mapExcept joinExcept m) `shouldBe` joinExcept (joinExcept m)
        it "\"joinF      (wrapF m)  =  m\" test" $ joinExcept (wrapExcept m) `shouldBe` m
        it "\"joinF (mapF wrapF m)  =  m\" test" $ joinExcept (mapExcept wrapExcept m) `shouldBe` m

propExcept :: TestTree
propExcept = allProps "Except" genExcept mapExcept wrapExcept distExcept joinExcept

genExcept :: Gen (Except String String)
genExcept = Gen.choice [genError, genSuccess]
  where
    genError = Error <$> genString
    genSuccess = Success <$> genString
