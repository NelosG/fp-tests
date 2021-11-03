module T1Spec
  ( tests
  ) where

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (testSpec)

import GHC.Natural (Natural)
import HW1.T1 (Day (..), afterDays, daysToParty, isWeekend, nextDay)

instance Eq Day where
    (==) Monday Monday       = True
    (==) Tuesday Tuesday     = True
    (==) Wednesday Wednesday = True
    (==) Thursday Thursday   = True
    (==) Friday Friday       = True
    (==) Saturday Saturday   = True
    (==) Sunday Sunday       = True
    (==)  _ _                = False

instance Enum Day where
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum n = toEnum $ n `mod` 7

  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

instance Bounded Day where
  minBound = Monday
  maxBound = Sunday

afterDaysHelper :: Natural -> Day -> Day
afterDaysHelper n d = toEnum $ fromIntegral n + fromEnum d

spec_Days :: Spec
spec_Days = do
  describe "nextDay tests" $ do
    it "nextDay Monday" $ nextDay Monday `shouldBe` Tuesday
    it "nextDay Tuesday" $ nextDay Tuesday `shouldBe` Wednesday
    it "nextDay Wednesday" $ nextDay Wednesday `shouldBe` Thursday
    it "nextDay Thursday" $ nextDay Thursday `shouldBe` Friday
    it "nextDay Friday" $ nextDay Friday `shouldBe` Saturday
    it "nextDay Saturday" $ nextDay Saturday `shouldBe` Sunday
    it "nextDay Sunday" $ nextDay Sunday `shouldBe` Monday
  describe "isWeekend tests" $ do
    it "isWeekend Monday" $ isWeekend Monday `shouldBe` False
    it "isWeekend Tuesday" $ isWeekend Tuesday  `shouldBe` False
    it "isWeekend Wednesday" $ isWeekend Wednesday `shouldBe` False
    it "isWeekend Thursday" $ isWeekend Thursday  `shouldBe` False
    it "isWeekend Friday" $ isWeekend Friday  `shouldBe` False
    it "isWeekend Saturday" $ isWeekend Saturday `shouldBe` True
    it "isWeekend Sunday" $ isWeekend Sunday `shouldBe` True
  describe "daysToParty tests" $ do
    it "daysToParty Monday" $ daysToParty Monday `shouldBe` 4
    it "daysToParty Tuesday" $ daysToParty Tuesday `shouldBe` 3
    it "daysToParty Wednesday" $ daysToParty Wednesday `shouldBe` 2
    it "daysToParty Thursday" $ daysToParty Thursday  `shouldBe` 1
    it "daysToParty Friday" $ daysToParty Friday `shouldBe` 0
    it "daysToParty Saturday" $ daysToParty Saturday `shouldBe` 6
    it "daysToParty Sunday" $ daysToParty Sunday  `shouldBe` 5

genNatural :: Gen Natural
genNatural = Gen.integral_ $ Range.linear 0 10000

genDay :: Gen Day
genDay = Gen.enumBounded

prop_Days :: Property
prop_Days = property $ do
  n <- forAll genNatural
  d <- forAll genDay
  afterDays n d === afterDaysHelper n d

hspecDays :: IO TestTree
hspecDays = testSpec "Days tests" spec_Days

propertyDays :: IO TestTree
propertyDays = return $ testProperty "Days afterDays" prop_Days

tests :: IO TestTree
tests = do
  unitTests <- hspecDays
  propTests <- propertyDays
  return $ testGroup "HW1.T1" [unitTests, propTests]
