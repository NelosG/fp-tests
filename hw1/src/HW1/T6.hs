module HW1.T6
       ( mcat,
       epart
       ) where


mcat :: Monoid a => [Maybe a] -> a

-- using foldable

-- ghci> mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"]
-- "monoid"

-- ghci> Data.Monoid.getSum $ mcat [Nothing, Just 2, Nothing, Just 40]
-- 42

-- | Concat all Either's to array

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)

-- ghci> epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
-- (Sum {getSum = 8},[1,2,3,4,5])
