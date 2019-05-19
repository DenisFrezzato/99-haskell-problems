-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (xs!!(k - 1), take (k - 1) xs ++ drop k xs)

removeAt' :: Int -> [a] -> (a, [a])
removeAt' k xs = (x, init ++ tail)
    where
        (init, x:tail) = splitAt (k - 1) xs

