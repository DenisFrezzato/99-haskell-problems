-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (head:xs) = if head == last xs then isPalindrome $ init xs else False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = foldl (\acc t -> acc && eqT t) True zipped
    where
        halfLength = length xs `div` 2
        firstHalf = take halfLength xs
        secondHalf = reverse $ drop halfLength xs
        zipped = zip firstHalf secondHalf
        eqT (x, y) = x == y
