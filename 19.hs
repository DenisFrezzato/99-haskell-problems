-- Rotate a list N places to the left. 
-- Hint: Use the predefined functions length and (++). 
rotate :: [a] -> Int -> [a]
rotate xs steps = drop maxSteps xs ++ take maxSteps xs
    where
        xCount = length xs
        safeSteps = if steps < 0 then xCount + steps else steps 
        maxSteps = safeSteps `mod` xCount
