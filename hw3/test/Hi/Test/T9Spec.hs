module Hi.Test.T9Spec (spec) where

import Text.RawString.QQ

import HW3.Action

import Hi.Test.Common
import qualified Data.Set as Set

import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
#if HI_TEST_UPTO < 9
	emptyTest
#else
	let testEvalIO = testEvalM $ unwrapHIO Set.empty
	describe "rand" do
		it "constant" do
			[r|rand(0, 5)|] ~=?? Ok [r|rand(0, 5)|]
			[r|rand(0, 5.5)|] ~=?? EvalError HiErrorInvalidArgument
			[r|rand(40, -40)|] ~=?? EvalError HiErrorInvalidArgument
		it "inclusive" do
			testEvalIO "rand(0, 0)!" `shouldBe` Ok "0"
			testEvalIO "rand(30, 30)!" `shouldBe` Ok "30"
		it "rand" $ hedgehog do
			lower <- forAll $ Gen.int (Range.linear 0 30)
			upper <- forAll $ Gen.int (Range.linear lower 40)
			let res = testEvalIO $ "rand(" ++ show lower ++ "," ++ show upper ++ ")!"
			case res of
				Ok s -> do
					let r = read s
					diff r (<=) upper
					diff r (>=) lower
				_ -> failure
		-- TODO check is uniformal...
#endif
