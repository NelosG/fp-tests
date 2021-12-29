{-# LANGUAGE CPP #-}

module Hi.Test.Common
	( module Hi.Test.Common
	, module Test.Hspec
	, module HW3.Base
	) where

import Control.Monad.Identity
import Control.Exception
import Test.Hspec
import Data.Function
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Set as Set
import System.IO.Unsafe
import Test.Hspec.Hedgehog (hedgehog, failure)

import HW3.Base
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty

#ifndef TEST_NO_HI_MONAD
import HW3.Action
#endif

data TestRes
	= ParseError String
	| EvalError HiError
	| Ok String
#if HI_TEST_UPTO >= 7
	| Perm HiPermission
#endif
	deriving (Show)

instance Eq TestRes where
	ParseError _ == ParseError _ = True
	Ok a == Ok b = ((==) `on` filter (/= '\n')) a b
	EvalError a == EvalError b = a == b
#if HI_TEST_UPTO >= 7
	Perm a == Perm b = a == b
#endif
	_ == _ = False

getParsed :: String -> Maybe HiExpr
getParsed = rightToMaybe . parse

#if HI_TEST_UPTO >= 7
instance HiMonad Identity where
	runAction = const $ return HiValueNull

unwrapHIO :: Set.Set HiPermission -> HIO EvalRes -> TestRes
unwrapHIO perm HIO {..} =
	unsafePerformIO do
		res <- (Right <$> runHIO perm)
			`catch` (\(PermissionRequired ex) -> return $ Left ex)
		case res of
			Left err -> return $ Perm err
			Right res -> return $ matchEval res
#endif

type EvalRes = Either HiError HiValue

matchEval :: EvalRes -> TestRes
matchEval (Left  err) = EvalError err
matchEval (Right res) = Ok $ show $ prettyValue res

testEvalM :: HiMonad him => (him EvalRes -> TestRes) -> String -> TestRes
testEvalM unwrap s =
	case parse s of
		Left err -> ParseError $ errorBundlePretty err
		Right expr -> unwrap $ eval expr

testEval :: String -> TestRes
testEval = testEvalM (matchEval . runIdentity)

infix 1 ~=??
(~=??) :: HasCallStack => String -> TestRes -> Expectation
b ~=?? a = testEval b `shouldBe` a

infix 1 ~=?!
(~=?!) :: String -> TestRes -> Expectation
b ~=?! a = testEval b `shouldNotBe` a

emptyTest :: Spec
emptyTest = describe "NOT IMPLEMENTED" $ it "fail" $ hedgehog failure
