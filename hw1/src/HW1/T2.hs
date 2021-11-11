module HW1.T2
  ( N (..)
  , nFromNatural
  , nToNum
  , ncmp
  , nmult
  , nplus
  , nsub
    -- * Advanced
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import GHC.Natural (Natural)

data N =
    Z
  | S N

nplus :: N -> N -> N        -- addition

nmult :: N -> N -> N        -- multiplication

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)

nFromNatural :: Natural -> N

nToNum :: Num a => N -> a

-- | Advanced

nEven, nOdd :: N -> Bool    -- parity checking

ndiv :: N -> N -> N         -- integer division

nmod :: N -> N -> N         -- modulo operation
