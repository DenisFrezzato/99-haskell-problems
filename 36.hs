import Data.List

-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity. 
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\fs -> (head fs, length fs)) . group . primeFactors

-- From 35.
primeFactors :: Int -> [Int]
primeFactors x = primeFactors' x primeNumbers 
    where
        primeFactors' :: Int -> [Int] -> [Int]
        primeFactors' x (p:ps)
            | p * p > x = [x]
            | otherwise = if x `mod` p == 0
                then p : primeFactors' (x `div` p) (p:ps) 
                else primeFactors' x ps 

-- From 31.
isPrime :: Int -> Bool
isPrime x = not $ hasFactor x 2
    where
        hasFactor x factor  
            | factor > x `div` 2 = False
            | x `mod` factor == 0 = True
            | otherwise = hasFactor x (factor + 1)

primeNumbers :: [Int]
primeNumbers = [x | x <- [2..], isPrime x]

