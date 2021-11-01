module Test.Parser where

import HW2.T6
import Test.Expr
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog (Gen, Property, forAll, (===), property)
import HW2.T4 (Expr, Prim(..))
import HW2.T1 (Except(Success))

sameExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
sameExprProp genExpr showExprGen = property $ do
  e <- forAll genExpr
  es <- forAll $ showExprGen e
  parseExpr es === Success e

propParser :: IO TestTree
propParser = return $
  testGroup
    "Parser properties"
    [ testProperty "Val" $ sameExprProp genVal showFull
    , testProperty "Add Sub" $ sameExprProp (genExpr [Add, Sub]) showFull
    , testProperty "Add Sub Div Mul" $ sameExprProp (genExpr [Add, Sub, Div, Mul]) showFull
    , testProperty "Add Sub Div Mul. Extra spaces, parenthesis" $ sameExprProp (genExpr [Add, Sub, Div, Mul]) showExtra
    , testProperty "Sub. No parenthesis" $ sameExprProp (genExprBamboo [Sub]) showBamboo
    , testProperty "Div. No parenthesis" $ sameExprProp (genExprBamboo [Div]) showBamboo
    , testProperty "Sub Div. No parenthesis" $ sameExprProp genExprPriority showPriority ]
