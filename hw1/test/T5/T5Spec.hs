module T5Spec where

import Data.List.NonEmpty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import HW1.T5 ( joinWith, splitOn )

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
