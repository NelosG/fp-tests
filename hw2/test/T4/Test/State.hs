{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.State
  ( genState
  ) where

import HW2.T1 (Annotated (..))
import HW2.T4 (State (runS), wrapState)
import qualified Hedgehog as H

genState :: (H.Gen x) -> (H.Gen (State () x))
genState genX = wrapState <$> genX

instance Eq x => Eq (State () x) where
  (==) state1 state2 = case runS state1 () of
    x1 :# _ -> case runS state2 () of
      x2 :# _ -> x1 == x2

instance Show x => Show (State () x) where
  show state = show $ runS state ()

deriving instance (Show a, Show e) => Show (Annotated e a)
