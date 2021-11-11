{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.ExceptState
  ( genExceptState
  ) where

import HW2.T1 (Annotated (..), Except (..))
import HW2.T5 (ExceptState (runES), throwExceptState, wrapExceptState)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

genExceptState :: (H.Gen x) -> (H.Gen (ExceptState () () x))
genExceptState genX = Gen.choice [wrapExceptState <$> genX, throwExceptState <$> Gen.constant ()]

instance (Eq e, Eq x) => Eq (ExceptState e () x) where
  (==) state1 state2 = case s1 of
    Error e -> case s2 of
      Error e'  -> e == e'
      Success _ -> False
    Success (x1 :# _) -> case s2 of
      Success (x2 :# _) -> x1 == x2
      Error _           -> False
    where
      s1 = runES state1 ()
      s2 = runES state2 ()

instance (Show e, Show x) => Show (ExceptState e () x) where
  show state = show $ runES state ()

deriving instance (Show a, Show e) => Show (Annotated e a)
deriving instance (Show a, Show e) => Show (Except e a)
