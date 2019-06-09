-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates. 
split :: [a] -> Integer -> ([a], [a])
split xs l = inner xs l []
    where
        inner [] _ acc = (acc, [])
        inner xs'@(x:xs) l init
            | l == 0 = (init, xs')
            | otherwise = inner xs (l - 1) (init ++ [x])
