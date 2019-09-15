-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number
-- greater than 2 is the sum of two prime numbers

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
