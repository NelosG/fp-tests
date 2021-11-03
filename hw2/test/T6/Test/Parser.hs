module Test.Parser
  ( propParser
  ) where

import HW2.T1 (Except (..))
import HW2.T4 (Expr, Prim (..))
import HW2.T6 (parseExpr)
import Hedgehog (Gen, Property, failure, forAll, property, success, (===))
import Test.Expr (InvalidVariant (ExtraWord, FakeOperation, MissingOperand, MissingOperation, MissingParen),
                  convertToLeftAssoc, evalExprInt, genExpr, genExprBamboo, genExprInvalid,
                  genExprPriority, genExprPriorityAssoc, genVal, showBamboo, showExtra, showFull,
                  showInvalidExpr, showMinGen, showPriority)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

sameExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
sameExprProp exprGen showExprGen = property $ do
  e <- forAll exprGen
  es <- forAll $ showExprGen e
  case parseExpr es of
    (Success re) -> re === e
    (Error _)    -> failure

valueExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
valueExprProp exprGen showExprGen = property $ do
  e <- forAll exprGen
  es <- forAll $ showExprGen e
  case parseExpr es of
    (Success re) ->
      round (evalExprInt re) === round (evalExprInt e)
    (Error _) -> failure

sameLeftAssocExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
sameLeftAssocExprProp exprGen showExprGen = property $ do
  e <- forAll exprGen
  es <- forAll $ showExprGen e
  case parseExpr es of
    (Success re) ->
      convertToLeftAssoc re === convertToLeftAssoc e
    (Error _) -> failure

invalidExpr :: Gen Expr -> (Expr -> Gen String) -> Property
invalidExpr exprGen showExprGen = property $ do
  e <- forAll exprGen
  es <- forAll $ showExprGen e
  case parseExpr es of
    (Success _) -> failure
    (Error _)   -> success

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
    , testProperty "Sub Div. No parenthesis" $ sameExprProp genExprPriority showPriority
    , testProperty "Add Mul by value. No parenthesis" $ valueExprProp genExprPriorityAssoc showPriority
    , testProperty "Add Mul. No parenthesis" $ sameLeftAssocExprProp genExprPriorityAssoc showPriority
    , testProperty "Add Mul. Minimum parenthesis" $ sameLeftAssocExprProp (genExpr [Add, Mul]) showMinGen
    , testProperty "Add Mul Div Sub. Minimum parenthesis" $ sameLeftAssocExprProp (genExpr [Add, Mul, Div, Sub]) showMinGen
    , testProperty "Add Mul Div Sub. Invalid expression. Extra words" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr ExtraWord
    , testProperty "Add Mul Div Sub. Invalid expression. Missing parenthesis" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr MissingParen
    , testProperty "Add Mul Div Sub. Invalid expression. Missing operand" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr MissingOperand
    , testProperty "Add Mul Div Sub. Invalid expression. Missing operation" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr MissingOperation
    , testProperty "Add Mul Div Sub. Invalid expression. Missing operation" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr FakeOperation ]
