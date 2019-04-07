-- Reverse a list.
myReverse :: [a] -> [a]
-- myReverse = reverse
myReverse = foldl (\acc x -> x:acc) []

myReverse' :: [a] -> [a]
myReverse' = foldr (\x acc -> acc ++ [x]) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []
