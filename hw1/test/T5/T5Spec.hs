module T5Spec
  ( tests
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (testSpec)

import HW1.T5 (joinWith, splitOn)

spec_splitOn :: Spec
spec_splitOn = do
  describe "Statement tests" $ do
    it "statement test 1" $ splitOn '/' "path/to/file" `shouldBe` ("path" :| ["to", "file"])
    it "statement test 2" $ splitOn '/' "path/with/trailing/slash/" `shouldBe` ("path" :| ["with", "trailing", "slash", ""])

hspecSplitOn :: IO TestTree
hspecSplitOn = testSpec "splitOn tests" spec_splitOn

spec_joinWith :: Spec
spec_joinWith = do
  describe "Statement tests" $ do
    it "statement test 1" $ joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"
    it "statement test 2" $ joinWith '/' ("path" :| ["with", "trailing", "slash", ""]) `shouldBe` "path/with/trailing/slash/"

hspecJoinWith :: IO TestTree
hspecJoinWith = testSpec "joinWith tests" spec_joinWith

genString :: Gen String
genString = Gen.subsequence $ '/' : ['a'..'z']

prop_id :: Property
prop_id = property $ do
  str <- forAll genString
  (joinWith '/' . splitOn '/') str === str

propertyId :: IO TestTree
propertyId = return $ testProperty "id property" prop_id

tests :: IO TestTree
tests = do
  specSplitOn <- hspecSplitOn
  specJoinWith <- hspecJoinWith
  propId <- propertyId
  return $ testGroup "HW1.T5" [specSplitOn, specJoinWith, propId]
