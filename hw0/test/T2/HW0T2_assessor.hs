{-# LANGUAGE TypeOperators, ExplicitNamespaces, TemplateHaskell #-}

import Data.Type.Equality ((:~:)(Refl))
import Data.Void (Void, absurd)
import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (Exception, throw, catch, evaluate)
import qualified Test.QuickCheck as QC

---------------------------
------ NAME CHECKING ------
---------------------------

import HW0.T2 (Not)
import HW0.T2 (doubleNeg)
import HW0.T2 (reduceTripleNeg)

---------------------------
------ TYPE CHECKING ------
---------------------------

type_Not :: Not a :~: (a -> Void)
type_Not = Refl

doubleNeg' :: a -> Not (Not a)
doubleNeg' = doubleNeg

reduceTripleNeg' :: Not (Not (Not a)) -> Not a
reduceTripleNeg' = reduceTripleNeg

---------------------------
------ PROP CHECKING ------
---------------------------

data TestExc = E
  deriving Show

instance Exception TestExc

checkTestExc :: Void -> Bool
checkTestExc v = unsafePerformIO $
  evaluate (absurd v) `catch` (\E -> return True)
{-# NOINLINE checkTestExc #-}

prop_doubleNeg :: Bool
prop_doubleNeg =
  checkTestExc $ doubleNeg (throw E) absurd

prop_reduceTripleNeg :: Bool
prop_reduceTripleNeg =
  checkTestExc $ reduceTripleNeg ($ absurd) (throw E)

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
