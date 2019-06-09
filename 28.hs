import Data.List
import qualified Data.IntMap as IntMap

-- Sorting a list of lists according to length of sublists

-- We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa. 
lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

-- Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according
-- to their length frequency; i.e., in the default, where sorting is done ascendingly,
-- lists with rare lengths are placed first, others with a more frequent length come later. 
lfsort :: [[a]] -> [[a]]
lfsort xs = let lengthFrequency = foldl (\acc x -> IntMap.insertWith (+) (length x) 1 acc) IntMap.empty xs
    in sortBy (\x y -> compare (lengthFrequency IntMap.! length x) (lengthFrequency IntMap.! length y)) xs
