{-# LANGUAGE StandaloneDeriving #-}

module Test.TAnnotated where

import HW2.T1 (Annotated ((:#)), mapAnnotated)
import HW2.T2
import HW2.T3
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

deriving instance (Show a, Show e) => Show (Annotated e a)
deriving instance (Eq a, Eq e) => Eq (Annotated e a)


hspecAnnotated :: IO TestTree
hspecAnnotated = testSpec "Annotated tests:" $ do
    let m = ((1 :# "hello") :# " I'm ") :# "NelosG"
    it "Base test" $ joinAnnotated (joinAnnotated m) `shouldBe` (1 :# "hello I'm NelosG")
    it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ joinAnnotated (mapAnnotated joinAnnotated m) `shouldBe` joinAnnotated (joinAnnotated m)
    it "\"joinF      (wrapF m)  =  m\" test" $ joinAnnotated (wrapAnnotated m) `shouldBe` m
    it "\"joinF (mapF wrapF m)  =  m\" test" $ joinAnnotated (mapAnnotated wrapAnnotated m) `shouldBe` m
