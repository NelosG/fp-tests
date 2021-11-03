module T7Spec
  ( tests
  ) where

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import HW1.T7 (DotString (..), Fun (..), Inclusive (..), ListPlus (..))

instance Eq a => Eq (ListPlus a) where
  (Last x) == (Last y) = x == y
  (x :+ xs) == (y :+ ys)
    | x == y = xs == ys
    | otherwise = False
  (==) _ _ = False



instance Eq DotString where
  (DS a) == (DS b) = a == b


instance (Eq a, Eq b) => Eq (Inclusive a b) where
  (This a) == (This b)       = a == b
  (That a) == (That b)       = a == b
  (Both a b) == (Both a' b') = a == a' && b == b'
  (==) _ _                   = False


listPlusFromList :: [a] -> ListPlus a
listPlusFromList [x]    = Last x
listPlusFromList (x:xs) = x :+ listPlusFromList xs
listPlusFromList _      = undefined

genList :: Gen [Int]
genList = Gen.list (Range.linear 1 500) Gen.enumBounded

prop_listPlus :: Property
prop_listPlus = property $ do
  a <- forAll genList
  b <- forAll genList
  c <- forAll genList
  let aPlus = listPlusFromList a
      bPlus = listPlusFromList b
      cPlus = listPlusFromList c
  (aPlus <> bPlus) <> cPlus === aPlus <> (bPlus <> cPlus)

propertyListPlus :: IO TestTree
propertyListPlus = return $ testProperty "ListPlus property" prop_listPlus

genString :: Gen String
genString = Gen.string (Range.linear 1 100) Gen.alphaNum

genThis :: Gen (Inclusive String String)
genThis = do
  This <$> genString

genThat :: Gen (Inclusive String String)
genThat = do
  That <$> genString

genBoth :: Gen (Inclusive String String)
genBoth = do
  str1 <- genString
  Both str1 <$> genString

genInclusive :: Gen (Inclusive String String)
genInclusive = Gen.choice [genThis, genThat, genBoth]

prop_inclusive :: Property
prop_inclusive = property $ do
  a <- forAll genInclusive
  b <- forAll genInclusive
  c <- forAll genInclusive
  (a <> b) <> c === a <> (b <> c)

propertyInclusive :: IO TestTree
propertyInclusive = return $ testProperty "Inclusive property" prop_inclusive

genDotString :: Gen DotString
genDotString = do
  DS <$> genString

spec_DotString :: Spec
spec_DotString = do
  describe "Statement tests" $ do
    it "Statement test 1" $ DS "person" <> DS "address" <> DS "city" `shouldBe` DS "person.address.city"

hspecDotString :: IO TestTree
hspecDotString = testSpec "DotString spec" spec_DotString

prop_semigroupDotString :: Property
prop_semigroupDotString = property $ do
  a <- forAll genDotString
  b <- forAll genDotString
  c <- forAll genDotString
  (a <> b) <> c === a <> (b <> c)

propertySemigroupDotString :: IO TestTree
propertySemigroupDotString = return $ testProperty "DotString semigroup" prop_semigroupDotString

prop_monoidLeftDotString :: Property
prop_monoidLeftDotString = property $ do
  a <- forAll genDotString
  mempty <> a === a

propertyMonoidLeftDotString :: IO TestTree
propertyMonoidLeftDotString = return $ testProperty "DotString left monoid" prop_monoidLeftDotString

prop_monoidRightDotString :: Property
prop_monoidRightDotString = property $ do
  a <- forAll genDotString
  a <> mempty === a

propertyMonoidRightDotString :: IO TestTree
propertyMonoidRightDotString = return $ testProperty "DotString right monoid" prop_monoidRightDotString

add5 :: Fun Int
add5 = F $ \a -> a + 5

mul2 :: Fun Int
mul2 = F $ \a -> a * 2

div3 :: Fun Int
div3 = F $ \a -> a `div` 3

apply :: Fun a -> a -> a
apply (F f) = f

spec_Fun :: Spec
spec_Fun = do
  describe "Semigroup Fun" $ do
    it "Test 1" $ apply (mul2 <> add5) 16 `shouldBe` 42
    it "Test 2" $ apply (mul2 <> mul2) 10 `shouldBe` 40
    it "Test 3" $ apply ((mul2 <> add5) <> div3) 16 `shouldBe` apply (mul2 <> (add5 <> div3)) 16
  describe "Monoid Fun" $ do
    it "Test 1" $ apply (mempty <> add5) 10 `shouldBe` 15
    it "Test 2" $ apply (add5 <> mempty) 10 `shouldBe` 15

hspecFun :: IO TestTree
hspecFun = testSpec "Fun tests" spec_Fun

tests :: IO TestTree
tests = do
  listPlus <- propertyListPlus
  inclusive <- propertyInclusive
  dsSpec <- hspecDotString
  dsSemi <- propertySemigroupDotString
  dsLeftMonoid <- propertyMonoidLeftDotString
  dsRightMonoid <- propertyMonoidRightDotString
  fun <- hspecFun
  return $ testGroup "HW1.T7" [listPlus, inclusive, dsSpec, dsSemi, dsLeftMonoid, dsRightMonoid, fun]
