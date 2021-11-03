{-# LANGUAGE StandaloneDeriving #-}

module Test.TExcept
  ( propExcept
  ) where
import HW2.T1 (Except (..), mapExcept)
import HW2.T2 (distExcept, wrapExcept)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Common (allProps, genString)
import Test.Tasty (TestTree)

deriving instance ((Show a, Show b)) => Show (Except a b)
deriving instance ((Eq a, Eq b)) => Eq (Except a b)

propExcept :: TestTree
propExcept = allProps "Except" genString genExcept mapExcept wrapExcept distExcept

genExcept :: Gen (Except String String)
genExcept = Gen.choice [genError, genSuccess]
  where
    genError = Error <$> genString
    genSuccess = Success <$> genString
