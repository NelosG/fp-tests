module Hi.Test.T10Spec (spec) where

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
	describe "short-circuit" do
		it "&& ||" do
			"null || 1" ~=?? Ok "1"
			"1 || null" ~=?? Ok "1"
			"null && 1" ~=?? Ok "null"
			"1 && null" ~=?? Ok "null"
			[r|rand(0, 5.5)|] ~=?? EvalError HiErrorInvalidArgument
			[r|rand(40, -40)|] ~=?? EvalError HiErrorInvalidArgument
		it "echo return null" do
			testEvalIO [r|echo("you should not see it")|] `shouldBe` Ok [r|echo("you should not see it")|]
			testEvalIO [r|echo("you should not see it")!|] `shouldBe` Perm AllowWrite
			-- I have no idea how to test it
			testEvalM (unwrapHIO $ Set.fromList [AllowWrite]) [r|echo("you should see it")!|] `shouldBe` Ok "null"
#endif
