{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module T5Spec
  ( tests
  ) where

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (EvaluationError (..), ExceptState (ES, runES), eval, joinExceptState, mapExceptState,
               wrapExceptState)
import qualified Hedgehog as H
import Test.Expr (genFullExpr, genFullExprZero, stupidEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

hspecBaseTest :: IO TestTree
hspecBaseTest = testSpec "runES tests:" $ do
  it "Success test" $ runES (eval ((2 + 3 * 5 - 7) / 2)) [] `shouldBe` Success (5 :# [Div 10 2, Sub 17 7, Add 2 15, Mul 3 5])
  it "Error test" $ runES (eval (1 / (10 - 5 * 2))) [] `shouldBe` Error DivideByZero
-- Map, Wrap and Join tests were added at the request of the student, a single test is enough for correct work in any case
  it "Map test" $ runES (mapExceptState (+ 1) $ eval (2 + 3 * 5 - 7)) [Sub 17 7] `shouldBe` Success (11 :# [Sub 17 7, Add 2 15, Mul 3 5, Sub 17 7])
  it "Wrap test" $ runES (wrapExceptState 10 :: ExceptState EvaluationError [Prim Double] Double) [Sub 17 7] `shouldBe` Success (10 :# [Sub 17 7])
  it "Join test Success" $ do
    let s = ES (\(h : t) -> Success(eval (2 + 3 * 5 - 7) :# t))
    runES (joinExceptState s) [Add 459 207, Sub 332 228] `shouldBe` Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5, Sub 332 228])
  it "Join test Error" $ do
    let s = ES (\(h : t) -> Error DivideByZero)
    runES (joinExceptState s :: ExceptState EvaluationError [Prim Double] Double) [Add 459 207, Sub 332 228] `shouldBe` Error DivideByZero

-- This one is for test tests
--   it "Reverse List test" $ runES (eval ((2 + 3) * (7 - 5))) [] `shouldBe` Success (10 :# [Mul 5 2, Add 2 3, Sub 7 5])


prop_randomExpr :: H.Property
prop_randomExpr = H.property $ do
  expr <- H.forAll genFullExpr
  runES (eval expr) [] H.=== stupidEval expr []

prop_zeroExpr :: H.Property
prop_zeroExpr = H.property $ do
  expr <- H.forAll genFullExprZero
  runES (eval expr) [] H.=== stupidEval expr []

propRandomExpr :: IO TestTree
propRandomExpr = return $ testProperty "Non zero expressions" prop_randomExpr

propZeroExpr :: IO TestTree
propZeroExpr = return $ testProperty "Zero expressions" prop_zeroExpr

tests :: IO TestTree
tests = do
  base <- hspecBaseTest
  rand <- propRandomExpr
  zero <- propZeroExpr
  return $ testGroup "HW2.T5" [base, rand, zero]
