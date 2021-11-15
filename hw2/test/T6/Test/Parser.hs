{-# LANGUAGE LambdaCase #-}
module Test.Parser
  ( propParser
  ) where

import Control.Monad.Morph (hoist)
import System.Timeout (timeout)

import HW2.T1 (Except (..))
import HW2.T4 (Expr, Prim (..))
import HW2.T6 (parseExpr)
import Hedgehog (Gen, Property, failure, forAll, property, success, (===), PropertyT)
import Test.Expr (InvalidVariant (..),
                  convertToLeftAssoc, evalExprInt, genExpr, genExprBamboo, genExprInvalid,
                  genExprPriority, genExprPriorityAssoc, genVal, showBamboo, showExtra, showFull,
                  showInvalidExpr, showMinGen, showPriority)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

withTimeout :: String -> Int -> PropertyT IO () -> PropertyT IO ()
withTimeout expr len = hoist errorOut
  where
    errorOut :: IO a -> IO a
    errorOut m = timeout len m >>= \case
        Just a -> return a
        Nothing -> ioError . userError $ "Timeout exceeded: " ++ expr

timedTest :: (Expr -> String -> PropertyT IO ()) -> Gen Expr -> (Expr -> Gen String) -> Property
timedTest prop exprGen showExprGen = property $ do
  e <- forAll exprGen
  es <- forAll $ showExprGen e
  withTimeout es (20 * 1000000) (prop e es)

sameExprProp' :: Expr -> String -> PropertyT IO ()
sameExprProp' e es =
  case parseExpr es of
    (Success re) -> re === e
    (Error _)    -> failure

sameExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
sameExprProp = timedTest sameExprProp'

valueExprProp' :: Expr -> String -> PropertyT IO ()
valueExprProp' e es =
  case parseExpr es of
    (Success re) ->
      round (evalExprInt re) === round (evalExprInt e)
    (Error _) -> failure

valueExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
valueExprProp = timedTest valueExprProp'

sameLeftAssocExprProp' :: Expr -> String -> PropertyT IO ()
sameLeftAssocExprProp' e es =
  case parseExpr es of
    (Success re) ->
      convertToLeftAssoc re === convertToLeftAssoc e
    (Error _) -> failure

sameLeftAssocExprProp :: Gen Expr -> (Expr -> Gen String) -> Property
sameLeftAssocExprProp = timedTest sameLeftAssocExprProp'

invalidExpr' :: Expr -> String -> PropertyT IO ()
invalidExpr' e es = 
  case parseExpr es of
    (Success _) -> failure
    (Error _)   -> success

invalidExpr :: Gen Expr -> (Expr -> Gen String) -> Property
invalidExpr = timedTest invalidExpr'

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
    , testProperty "Add Mul Div Sub. Invalid expression. Missing operation" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr FakeOperation
    , testProperty "Add Mul Div Sub. Invalid expression. Incorrect doubles" $ invalidExpr (genExprInvalid [Add, Mul, Div, Sub]) $ showInvalidExpr IncorrectDouble
    ]
