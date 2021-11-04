{-# LANGUAGE FlexibleInstances #-}

module Test.TFun.Ident
  ( testIdentLeft
  , testIdentRight
  ) where

import HW2.T1 (Fun (..))
import HW2.T2 (distFun, wrapFun)
import Test.Hspec (Expectation, shouldBe)
import Test.TFun.Data (Function (..), getF)

instance (Eq a, Num a) => Eq (Function a) where
  (==) (Fn a _) (Fn b _) = a 228 == b 228

instance (Show a, Num a) => Show (Function a) where
  show (Fn a forShow) = show $ forShow ++ " 228 == " ++ show (a 228)

test1 :: (Eq a,Show a,Num a) => Fun (a, ()) (a, ()) -> String -> Fun a a -> String -> Expectation
test1 a aDefenition b bDefenition = Fn (getF1 a) aDefenition `shouldBe` Fn (getF b) bDefenition

test2 :: (Eq a,Show a,Num a) => Fun ((), a) ((), a) -> String -> Fun a a -> String -> Expectation
test2 a aDefenition b bDefenition = Fn (getF2 a) aDefenition `shouldBe` Fn (getF b) bDefenition

testIdentRight :: Expectation
testIdentRight = test1 (distFun (wrapFun 1, wrapFun ())) "(distFun (wrapFun 1, wrapFun ()))" (wrapFun 1) "(wrapFun 1)"

testIdentLeft :: Expectation
testIdentLeft = test2 (distFun (wrapFun (), wrapFun 2)) "(distFun (wrapFun (), wrapFun 2))" (wrapFun 2) "(wrapFun 2)"

getF1 :: Fun (a, ()) (a, ()) -> a -> a
getF1 (F a) x = fst (a ( x, ()))

getF2 :: Fun ((), a) ((), a) -> a -> a
getF2 (F a) x = snd (a ((), x))

