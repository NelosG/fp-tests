module Test.T1Except where
import HW2.T1 (Except (Error, Success), mapExcept)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance (Eq e, Eq a) => Eq (Except e a) where
    (==) (Error a) (Error b) = a == b
    (==) (Success a) (Success b) = a == b
    (==) _ _ = False



hspecExcept :: IO TestTree
hspecExcept = testSpec "Except tests:" $ do
    describe "Error tests:" $ do
        it "Error test" $ mapExcept (+ 1) (Error "bad news") `shouldBe` Error "bad news"
        it "Error(f . g) test" $ (mapExcept (+ 1) . mapExcept (* 10)) (Error "bad news") `shouldBe` Error "bad news"  
    -- describe "Success tests:" $ do
    --     it "Success test" $ mapExcept (+ 1) (Success 1) `shouldBe` Success 2 
    --     it "Success(f . g) test" $ (mapExcept (+ 1) . mapExcept (* 10)) (Success 1) `shouldBe` Success 11
