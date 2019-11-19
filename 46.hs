-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result
-- of their respective operations; e.g. and(A,B) will succeed, if and only
-- if both A and B succeed.

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not $ a && b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ a || b

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

impl' :: Bool -> Bool -> Bool
impl' a b = not a || b

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> [String]
table f = [show a <> " " <> show b <> " " <> show (f a b) | a <- [True, False], b <- [True, False]]

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable f = mapM_ putStrLn $ table f
