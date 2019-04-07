-- Find the last element of a list.
myLast :: [a] -> a
-- myLast = last 
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse