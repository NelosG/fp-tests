{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Test.T1Except where
import HW2.T1 (Except (Error, Success), mapExcept)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Hedgehog
import Test.Common
import qualified Hedgehog.Gen as Gen
import Test.Tasty.Hedgehog

deriving instance _ => Show (Except a b)
deriving instance _ => Eq (Except a b)

hspecExcept :: IO TestTree
hspecExcept = testSpec "Except tests:" $ do
    describe "Error tests:" $ do
        it "Error test" $ mapExcept (+ 1) (Error "bad news") `shouldBe` Error "bad news"
        it "Error(f . g) test" $ (mapExcept (+ 1) . mapExcept (* 10)) (Error "bad news") `shouldBe` Error "bad news"  
    -- describe "Success tests:" $ do
    --     it "Success test" $ mapExcept (+ 1) (Success 1) `shouldBe` Success 2 
    --     it "Success(f . g) test" $ (mapExcept (+ 1) . mapExcept (* 10)) (Success 1) `shouldBe` Success 11

genExcept :: Gen (Except Int Int)
genExcept = Gen.choice [genError, genSuccess]
  where
    genError = Error <$> genInt
    genSuccess = Success <$> genInt

propExcept :: TestTree
propExcept = testGroup "Except properties" [
    testProperty "Except id property" $ idProp genExcept mapExcept
  , testProperty "Except composition property" $ compProp genExcept mapExcept
  ]
