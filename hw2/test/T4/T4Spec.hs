{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module T4Spec
  ( tests
  ) where

import HW2.T1 (Annotated ((:#)))
import HW2.T4 (Prim (Add, Mul, Sub), State (runS), eval)
import qualified Hedgehog as H
import Test.Expr (genFullExpr, stupidEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

hspecBaseTest :: IO TestTree
hspecBaseTest = testSpec "runS tests:" $ do
  it "Base test" $ runS (eval (2 + 3 * 5 - 7)) [] `shouldBe` (10 :# [Sub 17 7, Add 2 15, Mul 3 5])

prop_randomExpr :: H.Property
prop_randomExpr = H.property $ do
  expr <- H.forAll genFullExpr
  runS (eval expr) [] H.=== stupidEval expr []

propRandomExpr :: IO TestTree
propRandomExpr = return $ testProperty "Random expressions" prop_randomExpr

tests :: IO TestTree
tests = do
  base <- hspecBaseTest
  rand <- propRandomExpr
  return $ testGroup "HW2.T4" [base, rand]
