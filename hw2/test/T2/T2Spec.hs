module T2Spec where

import Test.TAnnotated
import Test.Tasty (TestTree, testGroup)


tests :: IO TestTree
tests = do
    -- option <- hspecOption
    -- pair <- hspecPair
    -- quad <- hspecQuad
    -- annotated <- hspecAnnotated
    -- except <- hspecExcept
    -- prioritised <- hspecPrioritised
    -- -- stream <- hspecStream
    -- list <- hspecList
    -- fun <- funProp
    -- tree <- hspecTree
    return $ testGroup "HW2.T2" []
