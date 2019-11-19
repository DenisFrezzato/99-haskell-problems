-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result
-- of their respective operations; e.g. and(A,B) will succeed, if and only
-- if both A and B succeed.

and' :: Bool -> Bool -> Bool
and' = (&&)

infix 3 `and'`

or' :: Bool -> Bool -> Bool
or' = (||)

infix 2 `or'`

nand' :: Bool -> Bool -> Bool
nand' a b = not $ a && b

infix 3 `nand'`

nor' :: Bool -> Bool -> Bool
nor' a b = not $ a || b

infix 2 `nor'`

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

infix 2 `xor'`

impl' :: Bool -> Bool -> Bool
impl' a b = not a || b

infix 4 `impl'`

equ' :: Bool -> Bool -> Bool
equ' = (==)

infix 4 `equ'`

table :: (Bool -> Bool -> Bool) -> [String]
table f = [show a <> " " <> show b <> " " <> show (f a b) | a <- [True, False], b <- [True, False]]

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable f = mapM_ putStrLn $ table f
