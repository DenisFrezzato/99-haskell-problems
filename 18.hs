-- Extract a slice from a list. 
-- Given two indices, i and k, the slice is the list containing the elements between 
-- the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.  
slice :: [a] -> Int -> Int -> [a]
slice xs start end = drop (start - 1) $ take end xs
