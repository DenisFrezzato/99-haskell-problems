-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
-- pack = group
pack (x:xs) = foldl (\acc x -> if (last . last $ acc) == x
    then init acc ++ [last acc ++ [x]]
    else acc ++ [[x]]) [[x]] xs
