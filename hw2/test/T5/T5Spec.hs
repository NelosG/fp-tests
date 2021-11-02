{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module T5Spec where

import Test.Tasty
import HW2.T1
import HW2.T4 hiding (eval)
import HW2.T5
import Test.Tasty.Hspec


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
    it "Success test" $ runES (eval (2 + 3 * 5 - 7)) [] `shouldBe` Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])
    it "Error test" $ runES (eval (1 / (10 - 5 * 2))) [] `shouldBe` Error DivideByZero


tests :: IO TestTree
tests = do
    base <- hspecBaseTest
    return $ testGroup "HW2.T5" [base]