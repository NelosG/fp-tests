module Test.TStream where

import HW2.T1 (Stream (..))
import HW2.T2 (distStream, wrapStream)
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
        it "Test 1" $ takeToList 100 (distStream (ascStream, evenStream)) `shouldBe` zip (takeToList 100 ascStream) (takeToList 100 evenStream)
        it "Test 2" $ takeToList 100 (wrapStream 42) `shouldBe` replicate 100 42

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

prop_homo :: Property
prop_homo = property $ do
    n <- forAll genRange
    a <- forAll genInt
    b <- forAll genInt
    takeToList n (distStream (wrapStream a, wrapStream b)) === takeToList n (wrapStream (a, b))

eliminateRight :: [(a, (a, a))] -> [[a]]
eliminateRight = foldr (\(a, (b, c)) -> (:) [a, b, c]) []

eliminateLeft :: [((a, a), a)] -> [[a]]
eliminateLeft = foldr (\((a, b), c) -> (:) [a, b, c]) []

prop_assoc :: Property
prop_assoc = property $ do
    n <- forAll genRange
    p <- forAll genStream
    q <- forAll genStream
    r <- forAll genStream
    eliminateRight (takeToList n (distStream (p, distStream (q, r)))) === eliminateLeft (takeToList n (distStream (distStream (p, q), r)))

prop_leftIdentity :: Property
prop_leftIdentity = property $ do
    n <- forAll genRange
    q <- forAll genStream
    map snd (takeToList n (distStream (wrapStream (), q))) === takeToList n q

prop_rightIdentity :: Property
prop_rightIdentity = property $ do
    n <- forAll genRange
    p <- forAll genStream
    map fst (takeToList n (distStream (p, wrapStream ()))) === takeToList n p

hspecStream :: IO TestTree
hspecStream = testSpec "Stream spec" spec_stream

propertyHomo :: IO TestTree
propertyHomo = return $ testProperty "Homo" prop_homo

propertyAssoc :: IO TestTree
propertyAssoc = return $ testProperty "Assoc" prop_assoc

propertyLeftIdentity :: IO TestTree
propertyLeftIdentity = return $ testProperty "Left identity" prop_leftIdentity

propertyRightIdentity :: IO TestTree
propertyRightIdentity = return $ testProperty "Right identity" prop_rightIdentity

streamTests :: IO TestTree
streamTests = do
    spec <- hspecStream
    homo <- propertyHomo
    assoc <- propertyAssoc
    left <- propertyLeftIdentity
    right <- propertyRightIdentity
    return $ testGroup "Stream tests" [spec, homo, assoc, left, right]
