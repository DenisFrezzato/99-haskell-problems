-- Find the number of elements of a list.
myLength :: [a] -> Int
-- myLength = length
myLength [] = 0
myLength [_] = 1
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (const (+1)) 0

