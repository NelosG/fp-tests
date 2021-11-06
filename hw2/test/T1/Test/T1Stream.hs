module Test.T1Stream where

import HW2.T1 (Stream (..), mapStream)
import Hedgehog (Gen, Property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (property)
import qualified Hedgehog.Internal.Range as Range
import Test.Common (allProps)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

instance Show a => Show (Stream a) where
    show str = show $ takeToList 50 str

takeToList :: Int -> Stream a -> [a]
takeToList 0 _         = []
takeToList n (a :> as) = a : takeToList (n - 1) as

streamProducer :: Integer -> (Integer -> Integer) -> Stream Integer
streamProducer n next = helper n
    where helper n = n :> helper (next n)

ascStream :: Stream Integer
ascStream = streamProducer 0 (+1)

evenStream :: Stream Integer
evenStream = streamProducer 0 (+2)

fibStream :: Stream Integer
fibStream = 0 :> 1 :> helper 0 1
    where helper a b = (a + b) :> helper b (a + b)

dropStream :: Int -> Stream a -> Stream a
dropStream 0 as        = as
dropStream n (a :> as) = dropStream (n - 1) as

spec_stream :: Spec
spec_stream = do
    describe "Stream unit tests" $ do
        it "Test 1" $ takeToList 100 (mapStream (*2) ascStream) `shouldBe` takeToList 100 evenStream
        it "Test 2" $ takeToList 100 (mapStream (+10) evenStream) `shouldBe` map (+10) (takeToList 100 evenStream)

genRange :: Gen Int
genRange = Gen.int (Range.linear 100 1000)

genInt :: Gen Integer
genInt = Gen.integral (Range.linear 42 133700)

genStream :: Gen (Stream Integer)
genStream = do
    start <- genInt
    val <- genInt
    f <- Gen.choice [Gen.constant (+), Gen.constant (*), Gen.constant (-)]
    return $ streamProducer start (f val)

prop_id :: Property
prop_id = property $ do
    n <- forAll genRange
    str <- forAll genStream
    takeToList n (mapStream id str) === takeToList n str

prop_comp :: Property
prop_comp = property $ do
    n <- forAll genRange
    str <- forAll genStream
    takeToList n ((mapStream (+10) . mapStream (*2)) str) === takeToList n ((mapStream ((+10) . (*2))) str)

hspecStream :: IO TestTree
hspecStream = testSpec "Stream spec" spec_stream

propertyId :: IO TestTree
propertyId = return $ testProperty "Id property" prop_id

propertyComp :: IO TestTree
propertyComp = return $ testProperty "Comp property" prop_comp

streamTests :: IO TestTree
streamTests = do
    spec <- hspecStream
    idProp <- propertyId
    compProp <- propertyComp
    return $ testGroup "Stream tests" [spec, idProp, compProp]
