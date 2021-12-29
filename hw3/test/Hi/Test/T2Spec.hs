{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Hi.Test.T2Spec (spec) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog

import Data.Char (toLower)

import HW3.Base
import Hi.Test.Common

showLower = map toLower . show

boolList = [False, True]

spec :: Spec
spec = do
	describe "booleans" do
		it "constants" do
			mapM_
				((\b -> b ~=?? Ok b) . showLower)
				boolList
		it "not" do
			mapM_
				(\b -> "not(" ++ showLower b ++ ")" ~=?? Ok (showLower $ not b))
				boolList
		it "and or" do
			mapM_
				(\(b1, b2) -> do
					let suff = "(" ++ showLower b1 ++ "," ++ showLower b2 ++ ")"
					"and" ++ suff ~=?? Ok (showLower $ b1 && b2)
					"or" ++ suff ~=?? Ok (showLower $ b1 || b2)
				)
				[(x, y) | x <- boolList, y <- boolList ]
		it "lazines" do
			"and(false, div(1, 0))" ~=?? Ok "false"
			"or(true, div(1, 0))" ~=?? Ok "true"
			"and(true, div(1, 0))" ~=?? EvalError HiErrorDivideByZero
			"or(false, div(1, 0))" ~=?? EvalError HiErrorDivideByZero
	describe "Eq" do
		it "numbers" do
			"equals(3, mul(1.5, 2))" ~=?? Ok "true"
			"equals(3, mul(1, 2))" ~=?? Ok "false"
		it "booleans" do
			"equals(false, false)" ~=?? Ok "true"
			"equals(false, true)" ~=?? Ok "false"
		it "different types" do
			"equals(false, 0)" ~=?? Ok "false"
			"equals(1, true)" ~=?? Ok "false"
		it "functions" do
			"equals(add, add)" ~=?? Ok "true"
			"equals(add, sub)" ~=?? Ok "false"
	describe "Ord" do
		it "numbers"  $ hedgehog do
			n1 :: Int <- forAll $ Gen.integral $ Range.linear (negate 1000) 1000
			n2 :: Int <- forAll $ Gen.integral $ Range.linear (negate 1000) 1000
			let check s f = do
				let op = s ++ "(" ++ show n1 ++ ", " ++ show n2 ++ ")"
				annotate op
				testEval op === Ok (showLower $ n1 `f` n2)
			check "less-than" (<)
			check "greater-than" (>)
			check "not-less-than" (>=)
			check "not-greater-than" (<=)
			check "equals" (==)
			check "not-equals" (/=)
		it "functions" do
			let als = testEval "less-than(add, sub)"
			let sga = testEval "greater-than(sub, add)"
			als `shouldSatisfy` (`elem` [Ok "true", Ok "false"])
			sga `shouldSatisfy` (`elem` [Ok "true", Ok "false"])
			als `shouldBe` sga
			"or(less-than(add, mul), less-than(mul, add))" ~=?? Ok "true"
	describe "if" do
		it "basic" do
			"if(false, 0, 1)" ~=?? Ok "1"
			"if(true, 0, 1)" ~=?? Ok "0"
		it "laziness" do
			"if(true, 30, div(1, 0))" ~=?? Ok "30"
			"if(false, div(1, 0), 30)" ~=?? Ok "30"
		it "function-values" do
			"if(true, add, mul)" ~=?? Ok "add"
			"if(false, add, mul)(10, 10)" ~=?? Ok "100"

