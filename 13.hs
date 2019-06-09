data Encoded a = Multiple Int a | Single a deriving (Show)

-- Run-length encoding of a list (direct solution). 
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by replacing
-- the singleton lists (1 X) by X. 
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect = foldr encodeDirect' []
    where 
        encodeDirect' x [] = [Single x]
        encodeDirect' x (encodedY@(Single y):ys)
            | x == y = (Multiple 2 x):ys
            | otherwise = (Single x):encodedY:ys
        encodeDirect' x (encodedY@(Multiple l y):ys)
            | x == y = (Multiple (l + 1) x):ys
            | otherwise = (Single x):encodedY:ys
