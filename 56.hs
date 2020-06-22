-- Symmetric binary trees 
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node
-- and then the right subtree is the mirror image of the left subtree. Write a predicate
-- symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2
-- first to check whether one tree is the mirror image of another. We are only interested
-- in the structure, not in the contents of the nodes. 

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l r) (Branch _ l' r') = mirror l r && mirror l' r'
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r ) = mirror l r
