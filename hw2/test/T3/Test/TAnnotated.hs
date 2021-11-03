{-# LANGUAGE StandaloneDeriving #-}

module Test.TAnnotated
  ( hspecAnnotated
  , propAnnotated
  ) where

import HW2.T1 (Annotated ((:#)), mapAnnotated)
import HW2.T2 (distAnnotated, wrapAnnotated)
import HW2.T3 (joinAnnotated)
import Hedgehog (Gen)
import Test.Common (allProps, genString)
import Test.Hspec (it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a, Show e) => Show (Annotated e a)
deriving instance (Eq a, Eq e) => Eq (Annotated e a)


hspecAnnotated :: IO TestTree
hspecAnnotated = testSpec "Annotated tests:" $ do
  let m = ((1 :# "hello") :# " I'm ") :# "NelosG"
  it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinAnnotated (mapAnnotated joinAnnotated m) `shouldBe` joinAnnotated (joinAnnotated m)
  it "\"joinF      (wrapF m)  =  m\" test" $ joinAnnotated (wrapAnnotated m) `shouldBe` m
  it "\"joinF (mapF wrapF m)  =  m\" test" $ joinAnnotated (mapAnnotated wrapAnnotated m) `shouldBe` m

genAnnotated :: Gen (Annotated String String)
genAnnotated = (:#) <$> genString <*> genString

propAnnotated :: TestTree
propAnnotated = allProps "Annotated" genAnnotated mapAnnotated wrapAnnotated distAnnotated joinAnnotated
