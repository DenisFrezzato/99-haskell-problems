-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs times = concat [replicate times x | x <- xs] 

repli' :: [a] -> Int -> [a]
repli' xs times = concatMap (\x -> replicate times x) xs

repli'' :: [a] -> Int -> [a]
repli'' = flip $ concatMap . replicate
