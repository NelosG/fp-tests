module Test.TFun.Data where
    
import HW2.T1

data Function a = Fn (a -> a) String

getF :: Fun a b -> a -> b
getF (F a) = a