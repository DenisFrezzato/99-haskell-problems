import System.Random

-- Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return (rnd_select' gen xs n [])
    where
        rnd_select' _ _ 0 acc = acc
        rnd_select' gen xs n acc =
            let (randomIndex, newGen) = randomR (0, (length xs - 1)) gen
                (randomInit, x:randomTail) = splitAt randomIndex xs
            in rnd_select' newGen (randomInit ++ randomTail) (n - 1) acc ++ [x]
