-- Drop every N'th element from a list. 
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd . filter ((/= n) . fst) $ zip (cycle [1..n]) xs
