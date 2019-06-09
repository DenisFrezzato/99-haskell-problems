-- Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime x = not $ hasFactor x 2
    where
        hasFactor x factor  
            | factor > x `div` 2 = False
            | x `mod` factor == 0 = True
            | otherwise = hasFactor x (factor + 1)
