module Test.T1List(hspecList) where
import HW2.T1 (List (..), mapList)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

instance Eq a => Eq (List a) where
    (==) Nil Nil         = True
    (==) (a :. aTail) (b :. bTail) = a == b && aTail == bTail
    (==) _ _               = False

listFromStdList :: [a] -> List a
listFromStdList = foldr (:.) Nil

hspecList :: IO TestTree
hspecList = testSpec "List tests:" $ do
    describe "Nil tests:" $ do
        it "Nil test" $ mapList (+ 1) Nil `shouldBe` Nil
        it "Nil(f . g) test" $ (mapList (+ 1) . mapList (* 10)) Nil `shouldBe` Nil
    describe "List tests:" $ do
        it "List test1" $ mapList (+ 1) (listFromStdList [1, 2, 3]) `shouldBe` listFromStdList [2, 3, 4]
        it "List test2" $ mapList (+ 1) (listFromStdList [3, 1, 2]) `shouldBe` listFromStdList [4, 2, 3]
        it "List(f . g) test" $ (mapList (+ 1) . mapList (* 10)) (listFromStdList [1, 2, 3]) `shouldBe` listFromStdList [11, 21, 31]
