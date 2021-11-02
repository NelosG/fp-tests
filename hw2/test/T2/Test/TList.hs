{-# LANGUAGE StandaloneDeriving #-}

module Test.TList
  where

import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Test.Common
import Test.Tasty

deriving instance (Show a) => Show (List a)
deriving instance (Eq a) => Eq (List a)

propList :: TestTree
propList = allProps "List" genList mapList wrapList distList

listFromStdList :: [a] -> List a
listFromStdList = foldr (:.) Nil

genList :: Gen (List String)
genList = listFromStdList <$> genList'
  where
    genList' :: Gen [String]
    genList' = Gen.list (Range.linear 1 100) genString
