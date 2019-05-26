-- Insert an element at a given position into a list. 
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (i - 1) xs ++ [x] ++ drop (i - 1) xs

insertAt' :: a -> [a] -> Int -> [a]
insertAt' x xs i = let (init, tail) = splitAt (i - 1) xs in init ++ x:tail
        
