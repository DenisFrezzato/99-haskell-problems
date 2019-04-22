import Data.List

-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x]) 

dupli' :: [a] -> [a]
dupli' = foldr (\x acc -> x:x:acc) []
