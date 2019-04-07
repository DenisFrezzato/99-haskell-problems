-- Find the last but one element of a list. 
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:x':xs) = myButLast $ x':xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse
