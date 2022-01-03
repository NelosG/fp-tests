module T1.T1Spec (spec) where

import Data.Text (pack)
import Data.Void (Void)
import HW3.Base
import HW3.Parser
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Megaparsec.Error (ParseErrorBundle)

spec :: Spec
spec = do
  describe "Simple operations" $ do
    it "Adding" $ do
      (parse "add  ( 1,1  )  ") `shouldBe` (simpE HiFunAdd [toNumE 1, toNumE 1])
      (parse "  add   (     1   ,   1      )      ") `shouldBe` (simpE HiFunAdd [toNumE 1, toNumE 1])
      (parse "add  ( 6.25, -1.23456e-7  )  " `shouldBe` (simpE HiFunAdd [toNumE 6.25, toNumE (-1.23456e-7)]))
      (parse "add  ( -0.0, 1  )  ") `shouldBe` (simpE HiFunAdd [toNumE (-0.0), toNumE 1])

    it "Substraction" $ do
      (parse "sub  ( 1,1  )  ") `shouldBe` (simpE HiFunSub [toNumE 1, toNumE 1])
      (parse "  sub   (     1   ,   1      )      ") `shouldBe` (simpE HiFunSub [toNumE 1, toNumE 1])
      (parse "sub  ( 6.25, -1.23456e-7  )  " `shouldBe` (simpE HiFunSub [toNumE 6.25, toNumE (-1.23456e-7)]))
      (parse "sub  ( -0.0, 1  )  ") `shouldBe` (simpE HiFunSub [toNumE (-0.0), toNumE 1])

    it "Multiplication" $ do
      (parse "mul  ( 1,1  )  ") `shouldBe` (simpE HiFunMul [toNumE 1, toNumE 1])
      (parse "  mul   (     1   ,   1      )      ") `shouldBe` (simpE HiFunMul [toNumE 1, toNumE 1])
      (parse "mul  ( 6.25, -1.23456e-7  )  " `shouldBe` (simpE HiFunMul [toNumE 6.25, toNumE (-1.23456e-7)]))
      (parse "mul  ( -0.0, 1  )  ") `shouldBe` (simpE HiFunMul [toNumE (-0.0), toNumE 1])

    it "Division" $ do
      (parse "div  ( 1,1  )  ") `shouldBe` (simpE HiFunDiv [toNumE 1, toNumE 1])
      (parse "  div   (     1   ,   1      )      ") `shouldBe` (simpE HiFunDiv [toNumE 1, toNumE 1])
      (parse "div  ( 6.25, -1.23456e-7  )  " `shouldBe` (simpE HiFunDiv [toNumE 6.25, toNumE (-1.23456e-7)]))
      (parse "div  ( -0.0, 1  )  ") `shouldBe` (simpE HiFunDiv [toNumE (-0.0), toNumE 1])
  describe "Nested operations" $ do
    it "Deep 2" $ do
      (parse "add   ( mul   ( 1, 2  )  , 3  )  ") `shouldBe` (simpE HiFunAdd [simpEPure HiFunMul [toNumE 1, toNumE 2], toNumE 3])
      (parse "sub   ( 69, div   ( 1, 2  )    )  ") `shouldBe` (simpE HiFunSub [toNumE 69, simpEPure HiFunDiv [toNumE 1, toNumE 2]])
      (parse "add   ( mul   ( 23.3, 14  )  , div   ( 15, -2000  )    )  ") `shouldBe` (simpE HiFunAdd [simpEPure HiFunMul [toNumE 23.3, toNumE 14], simpEPure HiFunDiv [toNumE 15, toNumE (-2000)]])
    it "Deep 3" $ do
      (parse $ deepExprString 3 "add") `shouldBe` (Right $ deepExpr 3 HiFunAdd)
      (parse $ deepExprString 3 "sub") `shouldBe` (Right $ deepExpr 3 HiFunSub)
      (parse $ deepExprString 3 "mul") `shouldBe` (Right $ deepExpr 3 HiFunMul)
      (parse $ deepExprString 3 "div") `shouldBe` (Right $ deepExpr 3 HiFunDiv)
    it "Deep 13" $ do
      (parse $ deepExprString 13 "add") `shouldBe` (Right $ deepExpr 13 HiFunAdd)
      (parse $ deepExprString 13 "sub") `shouldBe` (Right $ deepExpr 13 HiFunSub)
      (parse $ deepExprString 13 "mul") `shouldBe` (Right $ deepExpr 13 HiFunMul)
      (parse $ deepExprString 13 "div") `shouldBe` (Right $ deepExpr 13 HiFunDiv)



deepExpr :: Int -> HiFun -> HiExpr
deepExpr d f =
  if d == 0
    then simpEPure f [toNumE 0.0, toNumE 0.0]
    else simpEPure f [deepExpr (d - 1) f, deepExpr (d - 1) f]

deepExprString :: Int -> String -> String
deepExprString d f =
  if d == 0
    then f ++ "  ( 0.0, 0.0  )  "
    else f ++ "  ( " ++ (deepExprString (d -1) f) ++ ", " ++ (deepExprString (d - 1) f) ++ "  )  "

simpE :: HiFun -> [HiExpr] -> Either (ParseErrorBundle String Void) HiExpr
simpE f args = Right (HiExprApply (toFun f) args)
simpEPure :: HiFun -> [HiExpr] -> HiExpr
simpEPure f args = (HiExprApply (toFun f) args)

notSimpE :: HiExpr -> [HiExpr] -> Either (ParseErrorBundle String Void) HiExpr
notSimpE f args = Right (HiExprApply f args)
notSimpEPure :: HiExpr -> [HiExpr] -> HiExpr
notSimpEPure f args = (HiExprApply f args)


toExpr :: HiValue -> HiExpr
toExpr v = HiExprValue $ v

toFun :: HiFun -> HiExpr
toFun x = HiExprValue $ HiValueFunction x

toNumV :: Rational -> HiValue
toNumV x = HiValueNumber x

toNumE :: Rational -> HiExpr
toNumE x = HiExprValue $ toNumV x

toBoolV :: Bool -> HiValue
toBoolV x = HiValueBool x

toBoolE :: Bool -> HiExpr
toBoolE x = HiExprValue $ toBoolV x

toStringV :: String -> HiValue
toStringV x = HiValueString (pack x)

toStringE :: String -> HiExpr
toStringE x = HiExprValue $ toStringV x

