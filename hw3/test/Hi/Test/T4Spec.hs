{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hi.Test.T4Spec
  ( spec
  ) where

import Text.RawString.QQ

import Control.Applicative (liftA2)
import Data.Ratio
import qualified Data.Text as T
import HW3.Base
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Range as Range
import Hi.Test.Common
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import Text.RawString.QQ
import Data.Semigroup (stimes, stimesIdempotent)

spec :: Spec
spec = do
  describe "strings" $ do
    it "constants" $ do
      "null" ~=?? Ok "null"
      [r|"Hello World"|] ~=?? Ok [r|"Hello World"|]
    it "patched-if" $ do
      "if (false, 1, 0)" ~=?? Ok "0"
      "if (true, 1, 0)" ~=?? Ok "1"
    it "from int-index" $ do
      [r|length("Hello World")|] ~=?? Ok "11"
      [r|to-upper("Hello World")|] ~=?? Ok [r|"HELLO WORLD"|]
      [r|to-lower("Hello World")|] ~=?? Ok [r|"hello world"|]
      [r|reverse("stressed")|] ~=?? Ok [r|"desserts"|]
      [r|trim("  Hello World  ")|] ~=?? Ok [r|"Hello World"|]
    it "operators" $ do
      [r|"Hello" + "World"|] ~=?? Ok [r|"HelloWorld"|]
      [r|"Cat" * 3|] ~=?? Ok [r|"CatCatCat"|]
      [r|"/dev" / "null"|] ~=?? Ok [r|"/dev/null"|]
    it "indexing" $ do
      [r|"kill me"(0)|] ~=?? Ok [r|"k"|]
      [r|"kill me"(length("kill me") - 1)|] ~=?? Ok [r|"e"|]
      [r|"kill me"(-100)|] ~=?? Ok "null"
      [r|"kill me"(30)|] ~=?? Ok "null"
      -- [r|"kill me"(0.5)|] ~=?? EvalError HiErrorInvalidArgument
    it "slicing" $ do
      [r|"Hello World"(0, 5)|] ~=?? Ok [r|"Hello"|]
      [r|"Hello World"(2, 4)|] ~=?? Ok [r|"ll"|]
      [r|"suicide"(4, 100)|] ~=?? Ok [r|"ide"|]
      [r|""(0, 0)|] ~=?? Ok [r|""|]
    it "slicing advanced/bonus" $ do
      "null == null" ~=?? Ok "true"
      "null /= null" ~=?? Ok "false"
      [r|"6 am"(2, null)|] ~=?? Ok [r|"am"|]
      [r|"6 am"(null, 3)|] ~=?? Ok [r|"6 a"|]
      [r|"lilmealone"(null, -1)|] ~=?? Ok [r|"lilmealon"|]
      [r|"aaa"(2, -2)|] ~=?? Ok [r|""|]
    it "int-index" $ do
      [r|to-upper("what a nice language")(7, 11)|] ~=?? Ok [r|"NICE"|]
      [r|"Hello" == "World"|] ~=?? Ok "false"
      [r|length("Hello" + "World")|] ~=?? Ok "10"
      [r|length("hehe" * 5) / 3|] ~=?? Ok "6 + 2/3"
    it "operators" $ hedgehog $ do
      s1@(HiValueString t1) <- forAll genString
      s2@(HiValueString t2) <- forAll genString
      n <- forAll $ Gen.integral $ Range.linear 0 100
      let makeOp op lhs rhs = HiExprApply (HiExprValue $ HiValueFunction op) (map HiExprValue [lhs, rhs])
      makeOp HiFunAdd s1 s2 ~=!! (Ok $ show $ t1 <> t2)
      makeOp HiFunDiv s1 s2 ~=!! (Ok $ show $ t1 <> T.pack "/" <> t2)
      makeOp HiFunMul s1 (HiValueNumber $ n % 1) ~=!! (Ok $ show $ if n == 0 then T.pack "" else stimes n t1)
    it "slice properties (advanced)" $ hedgehog $ do
      s@(HiValueString t) <- forAll genString
      HiExprValue s ~=!! (Ok $ show t)
      let slice start end = HiExprApply (HiExprValue s) (map HiExprValue [start, end])
      let toNum x = HiValueNumber $ x % 1
      let len = T.length t
      let checkFull start end = slice start end ~=!! (Ok $ show t)
      mapM_ (uncurry checkFull) $ (liftA2 (,)) [HiValueNull, toNum 0] [HiValueNull, toNum $ toInteger len] -- we don't do readable code here
      x <- forAll $ Gen.integral $ Range.linear 0 $ len
      slice (toNum $ toInteger x) HiValueNull ~=!! (Ok $ show (T.drop x t))
      slice HiValueNull (toNum $ toInteger x) ~=!! (Ok $ show (T.take x t))
