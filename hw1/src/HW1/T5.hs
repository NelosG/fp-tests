module HW1.T5
       ( splitOn,
       joinWith
       ) where

import Data.List.NonEmpty ( NonEmpty(..) )

splitOn :: Eq a => a -> [a] -> NonEmpty [a]


-- ghci> splitOn '/' "path/to/file"
-- ["path", "to", "file"]

-- ghci> splitOn '/' "path/with/trailing/slash/"
-- ["path", "with", "trailing", "slash", ""]

joinWith :: a -> NonEmpty [a] -> [a]

-- (joinWith sep . splitOn sep)  ≡  id

-- ghci> "import " ++ joinWith '.' ("Data" :| "List" : "NonEmpty" : [])
-- "import Data.List.NonEmpty"
