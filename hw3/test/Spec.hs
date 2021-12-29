{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Tasty (defaultMain, testGroup)
import T1.T1Spec (spec_Task1, spec_Task2)


main :: IO ()
main = do
  t1 <- spec_Task1
  t2 <- spec_Task2
  defaultMain $ testGroup "Group" [t1,t2]
