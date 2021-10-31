module Test.Common where
  
import Hedgehog (Gen, Property, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInt :: Gen Int
genInt = Gen.int $ Range.linear 1 100

idProp :: ((Show (m Int), Eq (m Int))) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
idProp gen mapM = property $ do
    m <- forAll gen
    mapM id m === m

compProp :: ((Show (m Int), Eq (m Int))) => Gen (m Int)
  -> ((Int -> Int) -> (m Int -> m Int))
  -> Property
compProp gen mapM = property $ do
  m <- forAll gen
  let check f g = (mapM f . mapM g) m === mapM (f . g) m
  check (+2) (+2)
  check (+2) (*3)
  check (*3) (+2)
  check (*3) (*3)
