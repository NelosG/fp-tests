{-# LANGUAGE StandaloneDeriving #-}

module Test.TExcept
  where
import HW2.T1
import HW2.T2
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Common
import Test.Tasty

deriving instance ((Show a, Show b)) => Show (Except a b)
deriving instance ((Eq a, Eq b)) => Eq (Except a b)

propExcept :: TestTree
propExcept = allProps "Except" genExcept mapExcept wrapExcept distExcept

genExcept :: Gen (Except String String)
genExcept = Gen.choice [genError, genSuccess]
  where
    genError = Error <$> genString
    genSuccess = Success <$> genString
