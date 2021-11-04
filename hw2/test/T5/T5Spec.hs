{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module T5Spec
  ( tests
  ) where

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (EvaluationError (..), ExceptState (runES), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a, Show e) => Show (Annotated e a)
deriving instance (Eq a, Eq e) => Eq (Annotated e a)

deriving instance Show a => Show (Prim a)
deriving instance Eq a => Eq (Prim a)

deriving instance Show Expr
deriving instance Eq Expr

deriving instance Show EvaluationError
deriving instance Eq EvaluationError

deriving instance (Show a, Show e) => Show (Except e a)
deriving instance (Eq a, Eq e) => Eq (Except e a)

hspecBaseTest :: IO TestTree
hspecBaseTest = testSpec "runES tests:" $ do
  it "Success test" $ runES (eval ((2 + 3 * 5 - 7) / 2)) [] `shouldBe` Success (5 :# [Div 10 2, Sub 17 7, Add 2 15, Mul 3 5])
  it "Error test" $ runES (eval (1 / (10 - 5 * 2))) [] `shouldBe` Error DivideByZero

tests :: IO TestTree
tests = do
  base <- hspecBaseTest
  return $ testGroup "HW2.T5" [base]
