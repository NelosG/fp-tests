module Hi.Test.T8Spec (spec) where

import Text.RawString.QQ

import HW3.Action

import Hi.Test.Common
import qualified Data.Set as Set

spec :: Spec
spec = do
#if HI_TEST_UPTO < 8
	emptyTest
#else
	let testEvalIO = testEvalM . unwrapHIO . Set.fromList
	describe "time" do
		it "constant" do
			[r|parse-time("2021-01-01 00:00:00 UTC")|] ~=?? Ok [r|parse-time("2021-01-01 00:00:00 UTC")|]
			[r|now|] ~=?? Ok [r|now|]
		it "null" do
			[r|parse-time("30")|] ~=?? Ok [r|null|]
		it "ops" do
			[r|parse-time("2021-01-01 00:00:00 UTC") + 365 * 24 * 60 * 60|] ~=?? Ok [r|parse-time("2022-01-01 00:00:00 UTC")|]
			[r|parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC")|] ~=?? Ok [r|3.351843755|]
		it "permissions" do
			let banned b act = do
				testEvalIO
						(filter (/= b) [minBound..maxBound])
						act
					`shouldBe` Perm b
			banned AllowTime "now!"
			case testEvalIO [AllowTime] "now!" of
				Ok _ -> 'a' `shouldBe` 'a'
				a -> (show a) `shouldBe` "ok"
		-- TODO test now <= parse-now <= now
#endif
