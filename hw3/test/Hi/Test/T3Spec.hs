module Hi.Test.T3Spec (spec) where

import HW3.Base
import Hi.Test.Common

spec :: Spec
spec = do
	let
		insert3 s = "1 " ++ s ++ " 2 " ++ s ++ " 3"
		insert3r s = "1 " ++ s ++ " (2 " ++ s ++ " 3)"
	describe "infix" do
		it "from int-index" do
			"2 + 2" ~=?? Ok "4"
			"2 + 2 * 3" ~=?? Ok "8"
			"(2 + 2) * 3" ~=?? Ok "12"
			"2 + 2 * 3 == (2 + 2) * 3" ~=?? Ok "false"
			"10 == 2*5 && 143 == 11*13" ~=?? Ok "true"
		it "not eq" do
			"1 /= 1" ~=?? Ok "false"
			"1 /= 2" ~=?? Ok "true"
		it "minus" do
			"2-2" ~=?? Ok "0"
			"2--2" ~=?? Ok "4"
			"2---2" ~=?? ParseError ""
		it "infixn" do
			let check s = insert3 s ~=?? ParseError ""
			check "<"
			check "<="
			check ">"
			check ">="
			check "=="
			check "/="
		it "infixr" do
			let
				check s = do
					let p1 = getParsed $ insert3 s
					p1 `shouldNotBe` Nothing
					let p2 = getParsed $ insert3r s
					p1 `shouldBe` p2
			check "&&"
			check "||"
		it "&& || clang warning" do
			"true || false && div(1,0)" ~=?? Ok "true"
			"(true || false) && div(1,0)" ~=?? EvalError HiErrorDivideByZero
			"false && div(1,0) || true " ~=?? Ok "true"
