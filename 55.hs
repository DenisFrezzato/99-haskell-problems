-- Construct completely balanced binary trees 
-- In a completely balanced binary tree, the following property holds for every node: The number
-- of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.  
-- Write a function cbal-tree to construct completely balanced binary trees for a given number
-- of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x'
-- as information into all nodes of the tree. 

import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
    let (q, r) = (n - 1) `quotRem` 2 
     in [Branch 'x' left right | x <- [q .. q + r]
                               , left <- cbalTree x
                               , right <- cbalTree (n - x - 1)] 

