module T1.T2Spec (spec) where

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
    it "Booleans" $ do
      (parse "  not   (  true    )  ") `shouldBe` (simpE HiFunNot [toBoolE True])
      (parse "and  (  true  , false  )  ") `shouldBe` (simpE HiFunAnd [toBoolE True, toBoolE False])
      (parse "or  (  true  , false  )  ") `shouldBe` (simpE HiFunOr [toBoolE True, toBoolE False])
      (parse "less-than  (  true  , false  )  ") `shouldBe` (simpE HiFunLessThan [toBoolE True, toBoolE False])
      (parse "greater-than  (  true  , false  )  ") `shouldBe` (simpE HiFunGreaterThan [toBoolE True, toBoolE False])
      (parse "equals  (  true  , false  )  ") `shouldBe` (simpE HiFunEquals [toBoolE True, toBoolE False])
      (parse "not-less-than  (  true  , false  )  ") `shouldBe` (simpE HiFunNotLessThan [toBoolE True, toBoolE False])
      (parse "not-greater-than  (  true  , false  )  ") `shouldBe` (simpE HiFunNotGreaterThan [toBoolE True, toBoolE False])
      (parse "not-equals  (  true  , false  )  ") `shouldBe` (simpE HiFunNotEquals [toBoolE True, toBoolE False])
      (parse "if (true, false, not-less-than) " `shouldBe` (simpE HiFunIf [toBoolE True, toBoolE False, toFun HiFunNotLessThan]))
      (parse "if (true, false, not-less-than)(true, false) " `shouldBe` (notSimpE (simpEPure HiFunIf [toBoolE True, toBoolE False, toFun HiFunNotLessThan]) [toBoolE True, toBoolE False]))
  describe "Nested booleans" $ do
    it "Boolean deep < 3" $ do
      ((parse "  not   (   not   (  true    )    )  ") `shouldBe` (simpE HiFunNot [simpEPure HiFunNot [toBoolE True]]))
      ((parse "and  (  true  ,   not   (  true    )    )  ") `shouldBe` (simpE HiFunAnd [toBoolE True, simpEPure HiFunNot [toBoolE True]]))
      ((parse "or  ( or  (  true  , false  )  , false  )  ") `shouldBe` (simpE HiFunOr [simpEPure HiFunOr [toBoolE True, toBoolE False], toBoolE False]))
      ((parse "less-than  (  true  , and  (  true  ,   not   (  true    )    )    )  ") `shouldBe` (simpE HiFunLessThan [toBoolE True, simpEPure HiFunAnd [toBoolE True, simpEPure HiFunNot [toBoolE True]]]))
      ((parse "greater-than  (   not   (  true    )  ,   not   (  true    )    )  ") `shouldBe` (simpE HiFunGreaterThan [simpEPure HiFunNot [toBoolE True], simpEPure HiFunNot [toBoolE True]]))
      ((parse "equals  ( greater-than  (   not   (  true    )  ,   not   (  true    )    )  , greater-than  (    not    (   true     )  ,    not    (   true    )    )    )  ") `shouldBe` (simpE HiFunEquals [simpEPure HiFunGreaterThan [simpEPure HiFunNot [toBoolE True], simpEPure HiFunNot [toBoolE True]], simpEPure HiFunGreaterThan [simpEPure HiFunNot [toBoolE True], simpEPure HiFunNot [toBoolE True]]]))
      ((parse "not-less-than  (  true  , and  (  true  ,   not   (  true    )    )    )  ") `shouldBe` (simpE HiFunNotLessThan [toBoolE True, simpEPure HiFunAnd [toBoolE True, simpEPure HiFunNot [toBoolE True]]]))
      ((parse "not-greater-than   (    not    (    not    (   true      )     )   ,   not    (    not    (   true     )    )     )  ") `shouldBe` (simpE HiFunNotGreaterThan [simpEPure HiFunNot [simpEPure HiFunNot [toBoolE True]], simpEPure HiFunNot [simpEPure HiFunNot [toBoolE True]]]))
      ((parse " not-equals   (    not    (    not    (   true     )     )   , false  )  ") `shouldBe` (simpE HiFunNotEquals [simpEPure HiFunNot [simpEPure HiFunNot [toBoolE True]], toBoolE False]))
    it "Boolean deep 10" $ do
      ((parse $ deepExprString 10 "not-greater-than") `shouldBe` (Right $ deepExpr 10 HiFunNotGreaterThan))
      ((parse $ deepExprString 10 "not-less-than") `shouldBe` (Right $ deepExpr 10 HiFunNotLessThan))
      ((parse $ deepExprString 10 "not-equals") `shouldBe` (Right $ deepExpr 10 HiFunNotEquals))
      ((parse $ deepExprString 10 "equals") `shouldBe` (Right $ deepExpr 10 HiFunEquals))
      ((parse $ deepExprString 10 "and") `shouldBe` (Right $ deepExpr 10 HiFunAnd))
      ((parse $ deepExprString 10 "or") `shouldBe` (Right $ deepExpr 10 HiFunOr))
  describe "Strange if" $ do
    it "Strange if" $ do      
      (parse "if(true, equals, less-than)(if(false, true, false), if(false, not-greater-than, less-than)(5, 15))") 
      `shouldBe`
       (notSimpE (simpEPure HiFunIf [toBoolE True,toFun HiFunEquals,toFun HiFunLessThan]) [simpEPure HiFunIf [toBoolE False, toBoolE True, toBoolE False],notSimpEPure (simpEPure HiFunIf [toBoolE False, toFun HiFunNotGreaterThan, toFun HiFunLessThan]) [toNumE 5, toNumE 15] ])
      
        
   
          
  


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