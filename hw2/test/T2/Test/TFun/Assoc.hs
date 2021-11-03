{-# LANGUAGE FlexibleInstances #-}

module Test.TFun.Assoc
  ( testAssoc
  ) where
import HW2.T1 (Fun (..))
import HW2.T2 (distFun, wrapFun)
import Test.Hspec (Expectation, shouldBe)
import Test.TFun.Data (Function (..))

instance (Eq a, Num a) => Eq (Function (a, (a, a))) where
  (==) (Fn a _) (Fn b _) = a (228, (322, 123)) == b (228, (322, 123))

instance (Show a, Num a) => Show (Function (a, (a, a))) where
  show (Fn a forShow) = show $ forShow ++ " 228 == " ++ show (a (228, (322, 123)))

--If this test fails, then it will generate an incorrect error message (brackets will be placed incorrectly)
testAssoc :: Expectation
testAssoc = test (distFun (wrapFun 1, distFun (wrapFun 2, wrapFun 3))) "(distFun (wrapFun 1, distFun (wrapFun 2, wrapFun 3)))" (distFun (distFun (wrapFun 1, wrapFun 2), wrapFun 3)) "(distFun (distFun (wrapFun 1, wrapFun 2), wrapFun 3))"

test :: (Eq a,Show a,Num a) => Fun a (a, (a, a)) -> String -> Fun a ((a, a), a) -> String -> Expectation
test a aDefenition b bDefenition = Fn (getFF1 a) aDefenition `shouldBe` Fn (getFF2 b) bDefenition

-- it's vety very bad, but i don't know how to do it
getFF2 :: Fun a ((a, a), a) -> (a, (a, a)) -> (a, (a, a))
getFF2 (F a) (x, (y, z)) = swap $ a x

swap :: ((a, a), a) -> (a, (a, a))
swap ((a, b), c) = (a, (b, c))

getFF1 :: Fun a (a, (a, a)) -> (a, (a, a)) -> (a, (a, a))
getFF1 (F a) (x, (y, z)) = a x
