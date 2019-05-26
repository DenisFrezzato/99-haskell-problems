-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end = take (end - start + 1) $ iterate (+1) start
