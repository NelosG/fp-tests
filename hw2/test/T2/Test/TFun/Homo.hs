{-# LANGUAGE FlexibleInstances #-}

module Test.TFun.Homo
  ( testHomo
  ) where

import HW2.T1 (Fun (..))
import HW2.T2 (distFun, wrapFun)
import Test.Hspec (Expectation, shouldBe)
import Test.TFun.Data (Function (..), getF)
import Test.Tasty.Hspec (Expectation, shouldBe)

instance (Eq a, Num a) => Eq (Function (a, a)) where
  (==) (Fn a _) (Fn b _) = a (228, 322) == b (228, 322)

instance (Show a, Num a) => Show (Function (a, a)) where
  show (Fn a forShow) = show $ forShow ++ " 228 == " ++ show (a (228, 322))

test :: (Eq a,Show a,Num a) => Fun (a, a) (a, a) -> String -> Fun (a, a) (a, a) -> String -> Expectation
test a aDefenition b bDefenition = Fn (getF a) aDefenition `shouldBe` Fn (getF b) bDefenition

testHomo :: Expectation
testHomo = test (distFun (wrapFun 1, wrapFun 2)) "(distFun (wrapFun 1, wrapFun 2))" (wrapFun (1, 2)) "(wrapFun (1, 2))"
