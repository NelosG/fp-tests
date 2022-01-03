{-# LANGUAGE TypeOperators, ExplicitNamespaces, TemplateHaskell #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC

---------------------------
------ NAME CHECKING ------
---------------------------

import HW0.T1 (distrib)
import HW0.T1 (assocPair)
import HW0.T1 (assocEither)
import HW0.T1 (type (<->)(Iso))
import HW0.T1 (flipIso)
import HW0.T1 (runIso)

---------------------------
------ TYPE CHECKING ------
---------------------------

distrib' :: Either a (b, c) -> (Either a b, Either a c)
distrib' = distrib

iso' :: (a -> b) -> (b -> a) -> (a <-> b)
iso' = Iso

assocPair' :: (a, (b, c)) <-> ((a, b), c)
assocPair' = assocPair

assocEither' :: Either a (Either b c) <-> Either (Either a b) c
assocEither' = assocEither

---------------------------
------ PROP CHECKING ------
---------------------------

type EI = Either Int (Int, Int)

prop_distrib_Left :: Int -> Bool
prop_distrib_Left x =
  distrib (Left x :: EI)
    ==
  (Left x, Left x)

prop_distrib_Right :: (Int, Int) -> Bool
prop_distrib_Right (x, y) =
  distrib (Right (x, y) :: EI)
    ==
  (Right x, Right y)

prop_assocPair_forwards :: (Int, Int, Int) -> Bool
prop_assocPair_forwards (x, y, z) =
  runIso assocPair (x, (y, z))
    ==
  ((x, y), z)

prop_assocPair_backwards :: (Int, Int, Int) -> Bool
prop_assocPair_backwards (x, y, z) =
  runIso (flipIso assocPair) ((x, y), z)
    ==
  (x, (y, z))

type EE = Either Int (Either Int Int)

prop_assocEither_Left :: Int -> Bool
prop_assocEither_Left x =
  runIso assocEither (Left x :: EE)
    ==
  Left (Left x)

prop_assocEither_RightLeft :: Int -> Bool
prop_assocEither_RightLeft x =
  runIso assocEither (Right (Left x) :: EE)
    ==
  Left (Right x)

prop_assocEither_RightRight :: Int -> Bool
prop_assocEither_RightRight x =
  runIso assocEither (Right (Right x) :: EE)
    ==
  Right x

prop_assocEither_LeftLeft :: Int -> Bool
prop_assocEither_LeftLeft x =
  runIso (flipIso assocEither) (Left (Left x))
    ==
  (Left x :: EE)

prop_assocEither_LeftRight :: Int -> Bool
prop_assocEither_LeftRight x =
  runIso (flipIso assocEither) (Left (Right x))
    ==
  (Right (Left x) :: EE)

prop_assocEither_Right :: Int -> Bool
prop_assocEither_Right x =
  runIso (flipIso assocEither) (Right x)
    ==
  (Right (Right x) :: EE)

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
