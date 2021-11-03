module Test.TFun
  ( hspecFun
  ) where

import HW2.T1 (Fun (F), mapFun)
import HW2.T2 (wrapFun)
import HW2.T3 (joinFun)
import Test.Hspec (Expectation, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Expectation, it, shouldBe, testSpec)
import Text.Show (Show)

data Function a = Fn (a -> a) String

instance (Eq a, Num a) => Eq (Function a) where
  (==) (Fn a _) (Fn b _) = a 228 == b 228

instance (Show a, Num a) => Show (Function a) where
  show (Fn a forShow) = show $ forShow ++ " 228 == " ++ show (a 228)

getF :: Fun a b -> a -> b
getF (F a) = a


test :: (Eq a, Show a, Num a) => Fun a a -> String -> Fun a a -> String -> Expectation
test a aDefenition b bDefenition = Fn (getF a) aDefenition `shouldBe` Fn (getF b) bDefenition

hspecFun :: IO TestTree
hspecFun = testSpec "Fun tests:" $ do
  let m = F (\x -> F (\y -> F (+ 1)))
  let l = F (+ 1)
  it "Base test" $ test (joinFun (joinFun m)) "joinFun (joinFun m)" l "F (+ 1)"

  it "\"joinF (mapF joinF m)  =  joinF (joinF m)\" test" $ test (joinFun (mapFun joinFun m)) "joinFun (mapFun joinFun m)"
                                                                (joinFun (joinFun m)) "joinFun (joinFun m)"

  it "\"joinF      (wrapF m)  =  m\" test" $ test (joinFun (mapFun wrapFun l)) "joinFun (mapFun wrapFun l)"
                                                    l "l"

  it "\"joinF (mapF wrapF m)  =  m\" test" $ test (joinFun (wrapFun l)) "joinFun (mapFun wrapFun l)"
                                                    l "l"
