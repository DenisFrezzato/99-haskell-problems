import Data.List

-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive
-- integers r (1 <= r < m) that are coprime to m. 
totient :: Int -> Int
totient m = length [x | x <- [1..m - 1], coprime x m]

coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1
