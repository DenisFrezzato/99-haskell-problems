-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs times = concat [replicate times x | x <- xs] 

repli' :: [a] -> Int -> [a]
repli' xs times = concatMap (replicate times) xs

repli'' :: [a] -> Int -> [a]
repli'' = flip $ concatMap . replicate
