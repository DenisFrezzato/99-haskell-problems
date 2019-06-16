import Data.List

-- Calculate Euler's totient function phi(m) (improved). 
-- See problem 34 for the definition of Euler's totient function. If the list of the prime
-- factors of a number m is known in the form of problem 36 then the function phi(m)
-- can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list
-- of prime factors (and their multiplicities) of a given number m. Then phi(m)
-- can be calculated with the following formula: 
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
        --  (p2 - 1) * p2 ** (m2 - 1) * 
        --  (p3 - 1) * p3 ** (m3 - 1) * ...
totient :: Int -> Int
totient = foldl (\acc (f, c) -> acc * (f - 1 ) * f ^ (c - 1)) 1 . primeFactorsMult

totient' :: Int -> Int
totient' x = product [(f - 1 ) * f ^ (c - 1) | (f, c) <- primeFactorsMult x]

-- From 36.
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

