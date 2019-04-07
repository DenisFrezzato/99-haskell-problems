-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs i = xs!!(i - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs i = head $ drop (i - 1) xs

elementAt'' :: [a] -> Int -> a
elementAt'' xs i = last $ take i xs