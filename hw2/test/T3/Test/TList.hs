{-# LANGUAGE StandaloneDeriving #-}

module Test.TList
  where

import HW2.T1 (List (Nil, (:.)), mapList)
import HW2.T2
import HW2.T3
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

deriving instance Show a => Show (List a)
deriving instance Eq a => Eq (List a)

propList :: TestTree
propList = allProps "List" genList mapList wrapList distList joinList

listFromStdList :: [a] -> List a
listFromStdList = foldr (:.) Nil

genList :: Gen (List String)
genList = listFromStdList <$> genList'
  where
    genList' :: Gen [String]
    genList' = Gen.list (Range.linear 1 100) genString

(+=) :: List a -> List a -> List a
(+=) Nil b          = b
(+=) (a :. aTail) b = a :. (aTail += b)

hspecList :: IO TestTree
hspecList = testSpec "List tests:" $ do
    describe "Base tests:" $ do
        let l = 1 :. (2 :. Nil)
        let k = l :. Nil :: List (List Int)
        let m = k :. (k :. Nil)
        let res = l += l :: List Int
        it "Base test" $ joinList (joinList m) `shouldBe` res
        it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinList (mapList joinList m) `shouldBe` joinList (joinList m)
        it "\"joinF      (wrapF m)  =  m\" test" $ joinList (wrapList m) `shouldBe` m
        it "\"joinF (mapF wrapF m)  =  m\" test" $ joinList (mapList wrapList m) `shouldBe` m
    describe "Nill tests:" $ do
        let m = Nil :: List (List (List Int))
        it "Base test" $ joinList (joinList m) `shouldBe` Nil
        it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinList (mapList joinList m) `shouldBe` joinList (joinList m)
        it "\"joinF      (wrapF m)  =  m\" test" $ joinList (wrapList m) `shouldBe` m
        it "\"joinF (mapF wrapF m)  =  m\" test" $ joinList (mapList wrapList m) `shouldBe` m
    describe "List of Nills tests:" $ do
        let m = ((Nil :. (Nil :. Nil)) :. ((Nil :. (Nil :. Nil)) :. Nil)) :: List (List (List Int))
        it "Base test" $ joinList (joinList m) `shouldBe` Nil
        it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinList (mapList joinList m) `shouldBe` joinList (joinList m)
        it "\"joinF      (wrapF m)  =  m\" test" $ joinList (wrapList m) `shouldBe` m
        it "\"joinF (mapF wrapF m)  =  m\" test" $ joinList (mapList wrapList m) `shouldBe` m
