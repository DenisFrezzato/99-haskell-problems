-- Eight queens problem 
-- This is a classical problem in computer science. The objective is to place eight queens
-- on a chessboard so that no two queens are attacking each other; i.e., no two queens
-- are in the same row, the same column, or on the same diagonal.  
-- Hint: Represent the positions of the queens as a list of numbers 1..N.
-- Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen
-- in the second column is in row 2, etc. Use the generate-and-test paradigm. 

import Data.Function ((&))

queens :: Int -> [[Int]]
queens n = generate n
  where 
    generate :: Int -> [[Int]]
    generate 0 = [[]]
    generate i = [x:xs | xs <- generate $ i - 1, x <- [1 .. 8], isValid $ x:xs]

    isValid :: [Int] -> Bool
    isValid [] = True
    isValid (queen:qs) = not (elem queen qs || isSameDiag queen qs) && isValid qs

    isSameDiag :: Int -> [Int] -> Bool
    isSameDiag queen qs = zip [1..] qs
      & any (\(cDist, r) -> abs (queen - r) == cDist)
