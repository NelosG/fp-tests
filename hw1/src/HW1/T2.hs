module HW1.T2
       ( N(..),
       nplus,
       nmult,
       nsub,
       ncmp,
       nFromNatural,
       nToNum,
       --advanced (comment if don't want to do andvanced)
       nEven,
       nOdd,
       ndiv,
       nmod
       ) where

import GHC.Natural (Natural)

data N = Z | S N deriving Show


nplus :: N -> N -> N        -- addition


nmult :: N -> N -> N        -- multiplication


nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)



ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)



nFromNatural :: Natural -> N

nToNum :: Num a => N -> a


-- Advanced

nEven, nOdd :: N -> Bool    -- parity checking


ndiv :: N -> N -> N         -- integer division


nmod :: N -> N -> N         -- modulo operation
