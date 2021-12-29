module Hi.Test.T6Spec (spec) where

import Text.RawString.QQ
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog

import Hi.Test.Common

spec :: Spec
spec = do
	describe "bytes" do
		it "const" do
			"pack-bytes([ 3, 255, 158, 32 ])" ~=?? Ok "[# 03 ff 9e 20 #]"
			"[# 03 ff 9e 20 #]" ~=?? Ok "[# 03 ff 9e 20 #]"
			"[# 0 #]" ~=?? ParseError "1-place in bytes"
			"[# 001 #]" ~=?? ParseError ">2-place in bytes"
			"[# -0 #]" ~=?? ParseError "negative in bytes"
			"[#00#]" ~=?? Ok "[# 00 #]"
			"[##]" ~=?? Ok "[#  #]"
			"[#         #]" ~=?? Ok "[#  #]"
		it "overload" do
			"[# 00 ff #] + [# 01 e3 #]" ~=?? Ok "[# 00 ff 01 e3 #]"
			"[# 00 ff #] * 3" ~=?? Ok "[# 00 ff 00 ff 00 ff #]"
		it "packs" do
			"unpack-bytes([# 10 20 30 #])" ~=?? Ok "[ 16, 32, 48 ]"
		it "encoding" do
			[r|encode-utf8("Hello!")|] ~=?? Ok "[# 48 65 6c 6c 6f 21 #]"
			[r|decode-utf8([# 48 65 6c 6c 6f #])|] ~=?? Ok [r|"Hello"|]
			[r|decode-utf8([# c3 28 #])|] ~=?? Ok "null"
		it "zip" do
			[r|zip(encode-utf8("Hello, World!"))|] ~=?? Ok "[# 78 da f3 48 cd c9 c9 d7 51 08 cf 2f ca 49 51 04 00 1f 9e 04 6a #]"
			[r|zip(encode-utf8("Hello, World!" * 1000))|] ~=?? Ok "[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]"
			"unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" ~=?? Ok "[# 01 02 03 #]"
		it "serialization simple" do
			"deserialise(serialise(null))" ~=?? Ok "null"
			[r|deserialise(serialise("null"))|] ~=?? Ok [r|"null"|]
			"deserialise([# 30 #])" ~=?? Ok "null"
		it "serialization num" $ hedgehog do
			s <- forAll $ Gen.int (Range.linear 0 100)
			testEval ("deserialise(serialise(" ++ show s ++ "))") === Ok (show s)
		it "serialization str" $ hedgehog do
			s :: String <- forAll $ Gen.string (Range.linear 0 100) Gen.alpha
			testEval ("deserialise(serialise(" ++ show s ++ "))") === Ok (show s)
		it "int-index" do
			"pack-bytes(range(30, 40))" ~=?? Ok "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
			"decode-utf8([# 68 69 #] * 5)" ~=?? Ok [r|"hihihihihi"|]
		it "unzip . zip === id" $ hedgehog do
			s :: String <- forAll $ Gen.string (Range.linear 0 100) Gen.alpha
			case testEval $ "encode-utf8(" ++ show s ++ ")" of
				Ok res ->
					testEval ("unzip(zip(" ++ res ++ "))") === Ok res
				err -> annotate (show err) >> failure
		it "decode-utf8 . encode-utf8 === id" $ hedgehog do
			s :: String <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
			testEval ("decode-utf8(encode-utf8(" ++ show s ++ "))") === Ok (show s)
		it "indexing" do
			"[# de ad ba be #](2)" ~=?? Ok (show (0xba :: Int))
			-- "[# de ad ba be #](-1)" ~=?? Ok (show 0xbe)
			"[# de ad ba be #](1000)" ~=?? Ok "null"
		it "slicing" do
			"[# de ad ba be #](null, 2)" ~=?? Ok "[# de ad #]"
			"[# de ad ba be #](1, -1)" ~=?? Ok "[# ad ba #]"
