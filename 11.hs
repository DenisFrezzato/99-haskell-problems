import Data.List

-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates
-- are transferred as (N E) lists.
data Encoded a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map encode' . encode where
    encode' (1, x) = Single x
    encode' (l, x) = Multiple l x
