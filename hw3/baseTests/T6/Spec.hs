{-# LANGUAGE TemplateHaskell, NegativeLiterals, BlockArguments, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleInstances #-}

import Control.Monad (unless, (<=<))
import System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Numeric (showHex)
import Numeric.Natural (Natural)
import Data.Word (Word8)
import Data.Void (Void)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Text.Megaparsec (ParseErrorBundle)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Functor.Classes (Eq1(liftEq))
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.String (renderString)
import qualified Language.Haskell.TH as TH

---------------------------
------ NAME CHECKING ------
---------------------------

import HW3.Base (
  HiFun (
    HiFunDiv,
    HiFunMul,
    HiFunAdd,
    HiFunSub,
    HiFunNot,
    HiFunAnd,
    HiFunOr,
    HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunIf,
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim,
    HiFunList,
    HiFunRange,
    HiFunFold,
    --------------
    HiFunPackBytes,
    HiFunUnpackBytes,
    HiFunEncodeUtf8,
    HiFunDecodeUtf8,
    HiFunZip,
    HiFunUnzip,
    HiFunSerialise,
    HiFunDeserialise
  ))
import HW3.Base (HiValue(HiValueNumber, HiValueFunction, HiValueBool, HiValueNull, HiValueString, HiValueList, HiValueBytes))
import HW3.Base (HiExpr(HiExprValue, HiExprApply))
import HW3.Base (HiError(HiErrorInvalidArgument, HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorDivideByZero))
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

import qualified HW3.Base

---------------------------
------ COMPATIBILITY ------
---------------------------

$(do mcls <- TH.lookupTypeName "HW3.Base.HiMonad"
     case mcls of
       Nothing -> return []
       Just cls -> do
         let stubClause = TH.clause [] (TH.normalB [e| error "Identity: runAction" |]) []
         let runActionD = TH.funD (TH.mkName "runAction") [stubClause]
         instD <- TH.instanceD (TH.cxt []) [t|$(TH.conT cls) Identity|] [runActionD]
         return [instD])

---------------------------
------ TYPE CHECKING ------
---------------------------

hiValueBytes' :: ByteString -> HiValue
hiValueBytes' = HiValueBytes

hiFunPackBytes' :: HiFun
hiFunPackBytes' = HiFunPackBytes

hiFunUnpackBytes' :: HiFun
hiFunUnpackBytes' = HiFunUnpackBytes

hiFunEncodeUtf8' :: HiFun
hiFunEncodeUtf8' = HiFunEncodeUtf8

hiFunDecodeUtf8' :: HiFun
hiFunDecodeUtf8' = HiFunDecodeUtf8

hiFunZip' :: HiFun
hiFunZip' = HiFunZip

hiFunUnzip' :: HiFun
hiFunUnzip' = HiFunUnzip

hiFunSerialise' :: HiFun
hiFunSerialise' = HiFunSerialise

hiFunDeserialise' :: HiFun
hiFunDeserialise' = HiFunDeserialise

eval' :: HiExpr -> Identity (Either HiError HiValue)
eval' = eval

parse' :: String -> Either (ParseErrorBundle String Void) HiExpr
parse' = parse

---------------------------
------ PROP CHECKING ------
---------------------------

prop_basic_parse_cases :: Bool
prop_basic_parse_cases =
  "pack-bytes"   `parses_to` HiExprValue (HiValueFunction HiFunPackBytes) &&
  "unpack-bytes" `parses_to` HiExprValue (HiValueFunction HiFunUnpackBytes) &&
  "zip"          `parses_to` HiExprValue (HiValueFunction HiFunZip) &&
  "unzip"        `parses_to` HiExprValue (HiValueFunction HiFunUnzip) &&
  "encode-utf8"  `parses_to` HiExprValue (HiValueFunction HiFunEncodeUtf8) &&
  "decode-utf8"  `parses_to` HiExprValue (HiValueFunction HiFunDecodeUtf8) &&
  "serialise"    `parses_to` HiExprValue (HiValueFunction HiFunSerialise) &&
  "deserialise"  `parses_to` HiExprValue (HiValueFunction HiFunDeserialise) &&
  bytes_lit_parse_case [0x01, 0x3f, 0xec]

prop_parse_bytes_lit :: [Word8] -> Bool
prop_parse_bytes_lit = bytes_lit_parse_case

prop_basic_eval_cases :: Bool
prop_basic_eval_cases =
  "pack-bytes([ 3, 255, 158, 32 ])" `evaluates_to_bytes` BS.pack [0x03, 0xff, 0x9e, 0x20] &&
  "unpack-bytes([# 10 20 30 #])" `evaluates_to_list_num` [16, 32, 48] &&
  "encode-utf8(\"Hello!\")" `evaluates_to_bytes` BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x21] &&
  "decode-utf8([# 48 65 6c 6c 6f #])" `evaluates_to_str` Text.pack "Hello" &&
  "decode-utf8([# c3 28 #])" `evaluates_to_null` () &&
  "[# 00 ff #] + [# 01 e3 #]" `evaluates_to_bytes` BS.pack [0x00, 0xff, 0x01, 0xe3] &&
  "[# 00 ff #] * 3" `evaluates_to_bytes` BS.pack [0x00, 0xff, 0x00, 0xff, 0x00, 0xff] &&
  "[# 00 ff 01 e3 #](1)" `evaluates_to_num` 255 &&
  "[# 00 ff 01 e3 #](1,3)" `evaluates_to_bytes` BS.pack [0xff, 0x01]

prop_eval_bytes_index_in_bounds :: [Word8] -> Bool
prop_eval_bytes_index_in_bounds bs =
  and [
     mkExpr i `evaluates_to_num` toRational b
    | (i, b) <- zip [0..] bs
  ]
    where mkExpr i = bs' ++ "(" ++ show i ++ ")"
          bs' = showBytesLit bs

prop_eval_bytes_slice_in_bounds :: [Word8] -> Bool
prop_eval_bytes_slice_in_bounds bs =
  and [
     mkExpr i k `evaluates_to_bytes` slice i k
    | i <- [0 .. n], k <- [0 .. n-i]
  ]
    where mkExpr i k = bs' ++ show (i, i + k)
          bs' = showBytesLit bs
          n = length bs
          slice i k = BS.pack ((take k . drop i) bs)

prop_basic_session :: Bool
prop_basic_session =
  "pack-bytes(range(30, 40))" `results_in_bytes`
    ["1e", "1f", "20", "21", "22", "23", "24", "25", "26", "27", "28"]
    &&
  "zip(encode-utf8(\"Hello, World!\" * 1000))" `results_in_bytes`
    [ "78", "da", "ed", "c7", "31", "0d", "00", "20", "0c", "00", "30", "2b",
      "f0", "23", "64", "0e", "30", "00", "df", "92", "25", "f3", "7f", "a0",
      "82", "af", "fd", "1a", "37", "b3", "d6", "d8", "d5", "79", "66", "88",
      "88", "88", "88", "88", "88", "88", "88", "88", "88", "88", "88", "88",
      "88", "88", "88", "88", "88", "88", "88", "88", "88", "88", "88", "88",
      "fc", "c9", "03", "ca", "0f", "3b", "28" ]
    &&
  "decode-utf8([# 68 69 #] * 5)" `results_in_str` show "hihihihihi"
    &&
  "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" `results_in_bytes` ["01", "02", "03"]

-- QuickCheck wrapper
newtype QCW a = QCW a
  deriving newtype (Eq, Show)

instance QC.Arbitrary (QCW HiFun) where
  arbitrary = fmap QCW $
    QC.elements [
      HiFunDiv,
      HiFunMul,
      HiFunAdd,
      HiFunSub,
      HiFunNot,
      HiFunAnd,
      HiFunOr,
      HiFunLessThan,
      HiFunGreaterThan,
      HiFunEquals,
      HiFunNotLessThan,
      HiFunNotGreaterThan,
      HiFunNotEquals,
      HiFunIf,
      HiFunLength,
      HiFunToUpper,
      HiFunToLower,
      HiFunReverse,
      HiFunTrim,
      HiFunList,
      HiFunRange,
      HiFunFold,
      HiFunPackBytes,
      HiFunUnpackBytes,
      HiFunEncodeUtf8,
      HiFunDecodeUtf8,
      HiFunZip,
      HiFunUnzip,
      HiFunSerialise,
      HiFunDeserialise
    ]

genValue :: Int -> QC.Gen HiValue
genValue 0 =
  QC.oneof [
    pure HiValueNull,
    pure (HiValueList Seq.empty),
    fmap HiValueBool QC.arbitrary,
    fmap HiValueNumber QC.arbitrary,
    fmap (\(QCW fn) -> HiValueFunction fn) QC.arbitrary,
    fmap (\(QC.UnicodeString str) -> HiValueString (Text.pack str)) QC.arbitrary,
    fmap (\bytes -> HiValueBytes (BS.pack bytes)) QC.arbitrary
  ]
genValue n = do
  len <- QC.chooseInt (1, n)
  items <- QC.vectorOf len (genValue ((n - 1) `div` len))
  return $ HiValueList (Seq.fromList items)

shrinkValue :: HiValue -> [HiValue]
shrinkValue (HiValueBool b) = [HiValueBool b' | b' <- QC.shrink b]
shrinkValue (HiValueNumber x) = [HiValueNumber x' | x' <- QC.shrink x]
shrinkValue (HiValueString s) =
  [HiValueString (Text.pack s')
    | QC.UnicodeString s' <- QC.shrink (QC.UnicodeString (Text.unpack s))]
shrinkValue (HiValueBytes bs) =
  [HiValueBytes (BS.pack bs')
    | bs' <- QC.shrink (BS.unpack bs)]
shrinkValue (HiValueList items) =
  [HiValueList (Seq.fromList items')
    | items' <- QC.shrinkList shrinkValue (toList items)]
shrinkValue _ = []

instance QC.Arbitrary (QCW HiValue) where
  arbitrary = fmap QCW (genValue 100)
  shrink (QCW v) = fmap QCW (shrinkValue v)

prop_eval_serialise_roundtrip :: QCW HiValue -> Bool
prop_eval_serialise_roundtrip (QCW v) =
  case runIdentity (eval (applyFn1 HiFunSerialise (HiExprValue v))) of
    Left _ -> False
    Right vbytes@(HiValueBytes _) ->
      case runIdentity (eval (applyFn1 HiFunDeserialise (HiExprValue vbytes))) of
        Left _ -> False
        Right v' -> eqValue v v'
    Right _ -> False

prop_eval_zip_roundtrip :: [Word8] -> Bool
prop_eval_zip_roundtrip xs =
    case runIdentity (eval e) of
      Left _ -> False
      Right (HiValueBytes bs') -> bs == bs'
      Right _ -> False
  where
    bs = BS.pack xs
    e = applyFn1 HiFunUnzip (applyFn1 HiFunZip (HiExprValue (HiValueBytes bs)))

applyFn1 :: HiFun -> HiExpr -> HiExpr
applyFn1 fn arg = HiExprApply (HiExprValue (HiValueFunction fn)) [arg]

bytes_lit_parse_case :: [Word8] -> Bool
bytes_lit_parse_case bs =
  showBytesLit bs `parses_to` HiExprValue (HiValueBytes (BS.pack bs))

showBytesLit :: [Word8] -> String
showBytesLit bs =
    "[#" ++ foldr (\n s -> showByte n . (' ':) . s) id bs "#]"
  where
    showByte n = showHex (n `quot` 16) . showHex (n `rem` 16)

parses_to :: String -> HiExpr -> Bool
parses_to str expected =
  case parse str of
    Left _ -> False
    Right e -> eqExpr e expected

evaluates_to_num :: String -> Rational -> Bool
evaluates_to_num str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueNumber expected)

evaluates_to_str :: String -> Text -> Bool
evaluates_to_str str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueString expected)

evaluates_to_null :: String -> () -> Bool
evaluates_to_null str _ =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v HiValueNull

evaluates_to_bytes :: String -> ByteString -> Bool
evaluates_to_bytes str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueBytes expected)

evaluates_to_list_num :: String -> [Rational] -> Bool
evaluates_to_list_num str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right v -> eqValue v (HiValueList (Seq.fromList (map HiValueNumber expected)))

prettyprints_str_to :: Text -> String -> Bool
prettyprints_str_to s expected =
    ppr_str (HiValueString s) == expected
  where
    ppr_str = renderString . layoutCompact . prettyValue

prettyprints_bytes_to :: ByteString -> [String] -> Bool
prettyprints_bytes_to bs expected =
    case ppr_bytes (HiValueBytes bs) of
      Nothing -> False
      Just pstr -> pstr == expected
  where
    ppr_bytes =
      fmap words . (strip_suffix <=< strip_prefix) .
        renderString . layoutCompact . prettyValue
    strip_prefix ('[':'#':cs) = Just cs
    strip_prefix _ = Nothing
    strip_suffix "#]" = Just ""
    strip_suffix (c:cs) = fmap (c:) (strip_suffix cs)
    strip_suffix [] = Nothing

results_in_str :: String -> String -> Bool
results_in_str str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueString s) -> prettyprints_str_to s expected
        Right _ -> False

results_in_bytes :: String -> [String] -> Bool
results_in_bytes str expected =
  case parse str of
    Left _ -> False
    Right e ->
      case runIdentity (eval e) of
        Left _ -> False
        Right (HiValueBytes bs) -> prettyprints_bytes_to bs expected
        Right _ -> False

eqResult :: Either HiError HiValue -> Either HiError HiValue -> Bool
eqResult (Left err1) (Left err2) = eqError err1 err2
eqResult (Right v1) (Right v2) = eqValue v1 v2
eqResult _ _ = False

eqError :: HiError -> HiError -> Bool
eqError HiErrorInvalidArgument HiErrorInvalidArgument = True
eqError HiErrorInvalidFunction HiErrorInvalidFunction = True
eqError HiErrorArityMismatch HiErrorArityMismatch = True
eqError HiErrorDivideByZero HiErrorDivideByZero = True
eqError _ _ = False

eqFn :: HiFun -> HiFun -> Bool
eqFn HiFunDiv HiFunDiv = True
eqFn HiFunMul HiFunMul = True
eqFn HiFunAdd HiFunAdd = True
eqFn HiFunSub HiFunSub = True
eqFn HiFunNot HiFunNot = True
eqFn HiFunAnd HiFunAnd = True
eqFn HiFunOr HiFunOr = True
eqFn HiFunLessThan HiFunLessThan = True
eqFn HiFunGreaterThan HiFunGreaterThan = True
eqFn HiFunEquals HiFunEquals = True
eqFn HiFunNotLessThan HiFunNotLessThan = True
eqFn HiFunNotGreaterThan HiFunNotGreaterThan = True
eqFn HiFunNotEquals HiFunNotEquals = True
eqFn HiFunIf HiFunIf = True
eqFn HiFunLength HiFunLength = True
eqFn HiFunToUpper HiFunToUpper = True
eqFn HiFunToLower HiFunToLower = True
eqFn HiFunReverse HiFunReverse = True
eqFn HiFunTrim HiFunTrim = True
eqFn HiFunList HiFunList = True
eqFn HiFunRange HiFunRange = True
eqFn HiFunFold HiFunFold = True
eqFn HiFunPackBytes HiFunPackBytes = True
eqFn HiFunUnpackBytes HiFunUnpackBytes = True
eqFn HiFunEncodeUtf8 HiFunEncodeUtf8 = True
eqFn HiFunDecodeUtf8 HiFunDecodeUtf8 = True
eqFn HiFunZip HiFunZip = True
eqFn HiFunUnzip HiFunUnzip = True
eqFn HiFunSerialise HiFunSerialise = True
eqFn HiFunDeserialise HiFunDeserialise = True
eqFn _ _ = False

eqValue :: HiValue -> HiValue -> Bool
eqValue (HiValueNumber x1) (HiValueNumber x2) = x1 == x2
eqValue (HiValueFunction fn1) (HiValueFunction fn2) = eqFn fn1 fn2
eqValue (HiValueBool b1) (HiValueBool b2) = b1 == b2
eqValue (HiValueString s1) (HiValueString s2) = s1 == s2
eqValue (HiValueList xs1) (HiValueList xs2) = liftEq eqValue xs1 xs2
eqValue (HiValueBytes bs1) (HiValueBytes bs2) = bs1 == bs2
eqValue HiValueNull HiValueNull = True
eqValue _ _ = False

eqExpr :: HiExpr -> HiExpr -> Bool
eqExpr (HiExprApply fn1 args1) (HiExprApply fn2 args2) =
  eqExpr fn1 fn2 && liftEq eqExpr args1 args2
eqExpr (HiExprValue v1) (HiExprValue v2) =
  eqValue v1 v2
eqExpr _ _ = False

return []

main :: IO ()
main = do
  ok <- $(QC.quickCheckAll)
  unless ok exitFailure
