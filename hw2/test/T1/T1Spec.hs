module T1Spec where
import Test.T1Option ( hspecOption )
import Test.T1Pair ( hspecPair )
import Test.T1Quad ( hspecQuad )
import Test.T1Annotated ( hspecAnnotated )
import Test.T1Except ( hspecExcept )
import Test.T1Prioritised ( hspecPrioritised )
-- import Test.T1Stream
import Test.T1List ( hspecList )
import Test.T1Fun
import Test.T1Tree ( hspecTree )
import Test.Tasty ( testGroup, TestTree )


tests :: IO TestTree
tests = do
    option <- hspecOption
    pair <- hspecPair

    quad <- hspecQuad
    annotated <- hspecAnnotated
    except <- hspecExcept
    prioritised <- hspecPrioritised
    -- stream <- hspecStream
    list <- hspecList
    fun <- funProp
    tree <- hspecTree
    
    return $ testGroup "HW2.T1" [option, pair, quad, annotated, except, prioritised, list, fun, tree]
    