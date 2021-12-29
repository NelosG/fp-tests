module Hi.Test.T1Spec (spec) where

import HW3.Base
import Hi.Test.Common

spec :: Spec
spec = do
	describe "numbers" do
		it "constants" do
			"3.0" ~=?? Ok "3"
			"-30" ~=?? Ok "-30"
			"1e3" ~=?? Ok "1000"
		it "functions"do
			"mul(2, 10)" ~=?? Ok "20"
			"sub(1000, 7)" ~=?? Ok "993" -- ya umer prosti
			"div(3, 5)" ~=?? Ok "0.6"
		it "functions 2" do
			"add(500, 12)" ~=?? Ok "512"
			"sub(10, 100)" ~=?? Ok "-90"
			"mul(23, 768)" ~=?? Ok "17664"
			"div(57, 190)" ~=?? Ok "0.3"
			"div(add(mul(2, 5), 1), sub(11,6))" ~=?? Ok "2.2"
			"sub(mul(201, 11), 0.33)" ~=?? Ok "2210.67"
		it "trivial errors" do
			"add(0)" ~=?? EvalError HiErrorArityMismatch
			"0(0, 0)" ~=?? EvalError HiErrorInvalidFunction
			"add(0, add)" ~=?? EvalError HiErrorInvalidArgument
			"div(1, 0)" ~=?? EvalError HiErrorDivideByZero
		it "advanced parsing" do
			"div(0, 1)    (0, 1)" ~=?? EvalError HiErrorInvalidFunction
			"(add)(0, 1)" ~=?? Ok "1"
	describe "formating" do
		it "spaces" do
			"   add (   45  ,  -  15  )  " ~=?? Ok "30"
			"add(-30,-30)" ~=?? Ok "-60"
			"div((3),(1))" ~=?? Ok "3"
		it "function strings" do
			"add" ~=?? Ok "add"
		it "output" do
			"3.14" ~=?? Ok "3.14"
			"div(1, 3)" ~=?? Ok "1/3"
			"div(3, 3)" ~=?? Ok "1"
			"div(-3, 3)" ~=?? Ok "-1"
			"div(-1, 3)" ~=?? Ok "-1/3"
			"div(4, 3)" ~=?? Ok "1 + 1/3"
			"div(4, -3)" ~=?? Ok "-1 - 1/3"
			"div(1, 20)" ~=?? Ok "0.05"
