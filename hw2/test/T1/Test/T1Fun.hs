{-# LANGUAGE FlexibleInstances #-}

module Test.T1Fun
  ( funProp
  , propFun
  ) where
import HW2.T1 (Fun (F), mapFun)
import Hedgehog (Gen, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Common (allProps, genInt, genFun)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Show (Show)

data Function a b = Fn (a -> a) String

instance (Eq a, Num a) => Eq (Function a b) where
  (==) (Fn a _) (Fn b _) = a 228 == b 228

instance (Show a, Show b, Num a) => Show (Function a b) where
  show (Fn a forShow) = show $ forShow ++ " 228 == " ++ show (a 228)

checkFn :: (Num a) => Function a String -> Function a String
checkFn (Fn a b) = Fn (getF $ mapFun (+ 1) (F a)) ("mapFun (+ 1) (" ++ b ++ ")")

checkFn2 :: (Num a) => Function a String -> Function a String
checkFn2 (Fn a b)= Fn (getF ((mapFun (+ 1) . mapFun (* 10)) (F a))) ("((mapFun (+ 1) . mapFun (* 10)) (" ++ b ++ ")")

getF :: Fun a a -> a -> a
getF (F a) = a

test1 :: TestTree
test1 = testProperty "Fun test1" $ property $ do
  let myFunc = Fn id "\\x -> x"
  checkFn myFunc === Fn (+ 1) "(+ 1)"

test2 :: TestTree
test2 = testProperty "Fun(f . g) test" $ property $ do
  let myFunc = Fn ((+ 1) . (* 100)) "(+ 1) . (* 100)"
  checkFn myFunc === Fn ((+ 2) . (* 100)) "((+ 2) . (* 100))"

testFG :: TestTree
testFG = testProperty "Fun test1" $ property $ do
  let myFunc = Fn id "\\x -> x"
  checkFn2 myFunc === Fn ((+ 1) . (* 10)) "((+ 1) . (* 10))"

funProp :: IO TestTree
funProp = return $ testGroup "Fun tests:" [test1, test2, testFG]

instance Eq a => Eq (Fun Int a) where
  (==) (F f1) (F f2) = map f1 [-10..10] == map f2 [-10..10]

propFun :: TestTree
propFun = allProps "Fun" genFun mapFun
