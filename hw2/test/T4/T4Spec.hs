{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}

module T4Spec
  ( tests
  ) where
import HW2.T1 (Annotated (..))
import HW2.T4 (Expr (..), Prim (..), State (runS), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

deriving instance (Show a, Show e) => Show (Annotated e a)
deriving instance (Eq a, Eq e) => Eq (Annotated e a)

deriving instance Show a => Show (Prim a)
deriving instance Eq a => Eq (Prim a)

deriving instance Show Expr
deriving instance Eq Expr

hspecBaseTest :: IO TestTree
hspecBaseTest = testSpec "runS tests:" $ do
  it "Base test" $ runS (eval (2 + 3 * 5 - 7)) [] `shouldBe` (10 :# [Sub 17 7, Add 2 15, Mul 3 5])

tests :: IO TestTree
tests = do
  base <- hspecBaseTest
  return $ testGroup "HW2.T4" [base]
