{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hi.Test.T5Spec (spec) where

import Text.RawString.QQ

import Hi.Test.Common
import Text.RawString.QQ
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog
import Data.Sequence ((><), index, fromList)
import Data.Ratio ((%))
import Data.Semigroup (stimes)

spec :: Spec
spec = do
  describe "lists and ranges" $ do
    it "int-index const" $ do
      "list(1,2)" ~=?? Ok "[ 1, 2 ]"
      "range(5, 10.3)" ~=?? Ok "[ 5, 6, 7, 8, 9, 10 ]"
    it "expr in list literal" $ do
      "[1 + 2, div(3, 2)]" ~=?? Ok "[ 3, 1.5 ]"
    it "overload" $ do
      [r| length([1, true, "Hello"])|] ~=?? Ok "3"
      [r|reverse([1, true, "Hello"])|] ~=?? Ok [r|[ "Hello", true, 1 ]|]
      "[1, 2] + [3, 4]" ~=?? Ok "[ 1, 2, 3, 4 ]"
      "[1, 2] * 2" ~=?? Ok "[ 1, 2, 1, 2 ]"
    it "int-index slice" $ do
      [r|[0, true, false, "hello", "world"](2, 4)|] ~=?? Ok [r|[ false, "hello" ]|]
      "reverse(range(0.5, 70/8))" ~=?? Ok "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"
    it "int-index folds" $ do
      "fold(add, [11, 22, 33])" ~=?? Ok "66"
      "fold(mul, [11, 22, 33])" ~=?? Ok "7986"
      "fold(div, [11, 22, 33])" ~=?? Ok "1/66"
      "fold(add, [2, 5] * 3)" ~=?? Ok "21"
      "fold(mul, range(1, 10))" ~=?? Ok "3628800"
    it "empty fold" $ do
      "fold(add, [])" ~=?? Ok "null"
    it "advanced fold" $ do
      "fold(add, [3, 3])" ~=?? Ok "6"
      "fold([29, 30, 31, 32, 33], [1])" ~=?? Ok "1"
      "fold([29, 30, 31, 32, 33], [1, 3])" ~=?? Ok "[ 30, 31 ]"
    it "operator properties" $ hedgehog $ do
      s1@(HiValueList l1) <- forAll genSeq
      s2@(HiValueList l2) <- forAll genSeq
      n <- forAll $ Gen.integral $ Range.linear 1 100
      (makeBinaryOp HiFunAdd s1 s2) ~=!! (Ok $ showExpr $ HiExprValue . HiValueList $ l1 >< l2)
      (makeBinaryOp HiFunMul s1 (HiValueNumber $ n % 1)) ~=!! (Ok . showExpr . HiExprValue . HiValueList $ stimes n l1)
    it "indexing properties" $ hedgehog $ do
      s@(HiValueList l) <- forAll $ (HiValueList . fromList) <$> Gen.list (Range.linear 1 5) genValue
      n <- forAll $ Gen.integral $ Range.linear 0 $ (length l) - 1
      (HiExprApply (HiExprValue s) ([HiExprValue $ HiValueNumber $ (toInteger n) % 1]))
        ~=!! (Ok . showExpr . HiExprValue $ index l n)
