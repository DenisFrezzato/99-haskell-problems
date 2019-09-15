import Data.List

-- A list of prime numbers. 
-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range. 

primesR :: Int -> Int -> [Int]
primesR l u = filter isPrime [l..u]

-- From 31.
isPrime :: Int -> Bool
isPrime x = not $ hasFactor x 2
    where
        hasFactor x factor  
            | factor > x `div` 2 = False
            | x `mod` factor == 0 = True
            | otherwise = hasFactor x (factor + 1)
