-- Given a range of integers by its lower and upper limit,
-- print a list of all even numbers and their Goldbach composition. 

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l u = map goldbach $ filter even [l..u]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' l u a = filter (\(x, y) -> x >= a && y >= a) $ goldbachList l u

-- From 40.
goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primesR 2 (n - 2), y <- primesR 2 (n - 2), x + y == n]

-- From 31.
isPrime :: Int -> Bool
isPrime x = not $ hasFactor x 2
    where
        hasFactor x factor  
            | factor > x `div` 2 = False
            | x `mod` factor == 0 = True
            | otherwise = hasFactor x (factor + 1)

primesR :: Int -> Int -> [Int]
primesR l u = filter isPrime [l..u]
