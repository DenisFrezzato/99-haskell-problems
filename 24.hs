import System.Random
import Data.List

-- Lotto: Draw N different random numbers from the set 1..M.  
diff_select :: Int -> Int -> IO [Int]
diff_select n max = do
    gen <- getStdGen
    return $ take n $ randomRs (1, max) gen
