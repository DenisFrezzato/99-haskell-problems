import Data.List (tails)

-- Generate the combinations of K distinct objects chosen from the N elements of a list 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
    y:ys <- tails xs
    xs <- combinations (n - 1) ys
    return (y:xs)
