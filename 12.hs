import Data.List

-- Decode a run-length encoded list. 
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version. 
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decode' where
    decode' (Single x) = [x]
    decode' (Multiple l x) = replicate l x
