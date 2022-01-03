{-# LANGUAGE TypeOperators, ExplicitNamespaces, TemplateHaskell #-}

import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC

---------------------------
------ NAME CHECKING ------
---------------------------

import HW0.T5 (type Nat)
import HW0.T5 (nz)
import HW0.T5 (ns)
import HW0.T5 (nplus)
import HW0.T5 (nmult)
import HW0.T5 (nFromNatural)
import HW0.T5 (nToNum)

---------------------------
------ TYPE CHECKING ------
---------------------------

type_Nat :: Nat a :~: ((a -> a) -> a -> a)
type_Nat = Refl

nz' :: Nat a
nz' = nz

ns' :: Nat a -> Nat a
ns' = ns

nplus' :: Nat a -> Nat a -> Nat a
nplus' = nplus

nmult' :: Nat a -> Nat a -> Nat a
nmult' = nmult

nFromNatural' :: Natural -> Nat a
nFromNatural' = nFromNatural

nToNum' :: Num a => Nat a -> a
nToNum' = nToNum

---------------------------
------ PROP CHECKING ------
---------------------------

nsz = iterate ns nz
f ==! n = f (*2) 1 == 2 ^ n
infix 4 ==!

prop_nz :: Int -> Bool
prop_nz n = nz (+1) n == n

prop_ns :: QC.Positive (QC.Small Int) -> Bool
prop_ns (QC.Positive (QC.Small n)) =
  nsz !! n ==! n

prop_nplus :: QC.Positive (QC.Small Int) -> QC.Positive (QC.Small Int) -> Bool
prop_nplus (QC.Positive (QC.Small n)) (QC.Positive (QC.Small m)) =
  nplus (nsz !! n) (nsz !! m)
    ==!
  n + m

prop_nmult :: QC.Positive (QC.Small Int) -> QC.Positive (QC.Small Int) -> Bool
prop_nmult (QC.Positive (QC.Small n)) (QC.Positive (QC.Small m)) =
  nmult (nsz !! n) (nsz !! m)
    ==!
  n * m

prop_roundtrip :: QC.Positive Int -> Bool
prop_roundtrip (QC.Positive n) =
  nToNum (nFromNatural (fromIntegral n))
    ==
  n

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
