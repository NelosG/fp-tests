{-# LANGUAGE LambdaCase #-}

module T6Spec
  ( tests
  ) where

import Data.Foldable (Foldable (fold))
import Data.Monoid (Sum (Sum, getSum))
import HW1.T6 (epart, mcat)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

spec_mcat :: Spec
spec_mcat = do
  describe "Statement tests" $ do
    it "Statement test 1" $ mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"] `shouldBe` "monoid"
    it "Statement test 2" $ Data.Monoid.getSum (mcat [Nothing, Just 2, Nothing, Just 40]) `shouldBe` 42

hspecMcat :: IO TestTree
hspecMcat = testSpec "mcat tests" spec_mcat

spec_epart :: Spec
spec_epart = do
  describe "Statement tests" $ do
    it "Statement test 1" $ epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` (Sum 8, [1, 2, 3, 4, 5])

hspecEpart :: IO TestTree
hspecEpart = testSpec "epart tests" spec_epart

genString :: Gen String
genString = Gen.string (Range.linear 0 1000) Gen.alphaNum

genMaybe :: Gen [Maybe String]
genMaybe = Gen.list (Range.linear 1 100) $ Gen.maybe $ genString

genEither :: Gen [Either String String]
genEither = Gen.list (Range.linear 1 100) $ Gen.either_ genString genString

prop_mcat :: Property
prop_mcat = property $ do
  xs <- forAll genMaybe
  let f = \case
        Just s  -> s
        Nothing -> ""
      strs = map f xs
  mcat xs === fold strs

propertyMcat :: IO TestTree
propertyMcat = return $ testProperty "mcat property" prop_mcat

prop_epart :: Property
prop_epart = property $ do
  xs <- forAll genEither
  let f (Left s) (acc1, acc2)  = (s : acc1, acc2)
      f (Right s) (acc1, acc2) = (acc1, s : acc2)
      (s1, s2) = foldr f ([""], [""]) xs
  epart xs === (fold s1, fold s2)

propertyEpart :: IO TestTree
propertyEpart = return $ testProperty "epart property" prop_epart

tests :: IO TestTree
tests = do
  specMcat <- hspecMcat
  specEpart <- hspecEpart
  propMcat <- propertyMcat
  propEpart <- propertyEpart
  return $ testGroup "HW1.T6" [specMcat, propMcat, specEpart, propEpart]
