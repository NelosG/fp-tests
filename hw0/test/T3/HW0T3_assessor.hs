{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC

---------------------------
------ NAME CHECKING ------
---------------------------

import HW0.T3 (s)
import HW0.T3 (k)
import HW0.T3 (i)
import HW0.T3 (compose)
import HW0.T3 (contract)
import HW0.T3 (permute)

---------------------------
------ TYPE CHECKING ------
---------------------------

s' :: (a -> b -> c) -> (a -> b) -> (a -> c)
s' = s

k' :: a -> b -> a
k' = k

i' :: a -> a
i' = i

compose' :: (b -> c) -> (a -> b) -> (a -> c)
compose' = compose

contract' :: (a -> a -> b) -> (a -> b)
contract' = contract

permute' :: (a -> b -> c) -> (b -> a -> c)
permute' = permute

---------------------------
------ PROP CHECKING ------
---------------------------

prop_s :: Int -> Bool
prop_s x =
    s f g x == f x (g x)
  where
    f = subtract
    g = (`mod` 3)

prop_k :: Int -> Bool
prop_k x =
  k x (error "Do not force") == x

prop_i :: Int -> Bool
prop_i x =
  i x == x

prop_compose :: Int -> Bool
prop_compose x =
    compose f g x == f (g x)
  where
    f = (*3)
    g = (+2)

prop_contract :: Int -> Bool
prop_contract x =
    contract f x == f x x
  where
    f = (+)

prop_permute :: Int -> Int -> Bool
prop_permute x y =
    permute f x y == f y x
  where
    f = (-)

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
