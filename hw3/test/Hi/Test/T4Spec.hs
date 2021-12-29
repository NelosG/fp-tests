module Hi.Test.T4Spec (spec) where

import Text.RawString.QQ

import HW3.Base
import Hi.Test.Common

spec :: Spec
spec = do
	describe "strings" do
		it "constants" do
			"null" ~=?? Ok "null"
			[r|"Hello World"|] ~=?? Ok [r|"Hello World"|]
		it "patched-if" do
			"if (null, 1, 0)" ~=?? Ok "0"
			"if (false, 1, 0)" ~=?? Ok "0"
			"if (0, 1, 0)" ~=?? Ok "1"
			"if (1, 1, 0)" ~=?? Ok "1"
			"if (true, 1, 0)" ~=?? Ok "1"
			[r|if ("abc", 1, 0)|] ~=?? Ok "1"
		it "from int-index" do
			[r|length("Hello World")|] ~=?? Ok "11"
			[r|to-upper("Hello World")|] ~=?? Ok [r|"HELLO WORLD"|]
			[r|to-lower("Hello World")|] ~=?? Ok [r|"hello world"|]
			[r|reverse("stressed")|] ~=?? Ok [r|"desserts"|]
			[r|trim("  Hello World  ")|] ~=?? Ok [r|"Hello World"|]
		it "operators" do
			[r|"Hello" + "World"|] ~=?? Ok [r|"HelloWorld"|]
			[r|"Cat" * 3|] ~=?? Ok [r|"CatCatCat"|]
			[r|"/dev" / "null"|] ~=?? Ok [r|"/dev/null"|]
		it "indexing" do
			[r|"kill me"(0)|] ~=?? Ok [r|"k"|]
			[r|"kill me"(length("kill me") - 1)|] ~=?? Ok [r|"e"|]
			[r|"kill me"(-100)|] ~=?? Ok "null"
			[r|"kill me"(30)|] ~=?? Ok "null"
			[r|"kill me"(0.5)|] ~=?? EvalError HiErrorInvalidArgument
		it "slicing" do
			[r|"Hello World"(0, 5)|] ~=?? Ok [r|"Hello"|]
			[r|"Hello World"(2, 4)|] ~=?? Ok [r|"ll"|]
			[r|"suicide"(4, 100)|] ~=?? EvalError HiErrorInvalidArgument
			[r|""(0, 0)|] ~=?? Ok [r|""|]
		it "slicing advanced/bonus" do
			"null == null" ~=?? Ok "true"
			"null /= null" ~=?? Ok "false"
			[r|"6 am"(2, null)|] ~=?? Ok [r|"am"|]
			[r|"6 am"(null, 3)|] ~=?? Ok [r|"6 a"|]
			[r|"lilmealone"(null, -1)|] ~=?? Ok [r|"lilmealon"|]
			[r|"aaa"(2, -2)|] ~=?? EvalError HiErrorInvalidArgument
		it "int-index" do
			[r|to-upper("what a nice language")(7, 11)|] ~=?? Ok [r|"NICE"|]
			[r|"Hello" == "World"|] ~=?? Ok "false"
			[r|length("Hello" + "World")|] ~=?? Ok "10"
			[r|length("hehe" * 5) / 3|] ~=?? Ok "6 + 2/3"
