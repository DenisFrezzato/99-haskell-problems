import System.Random

-- Generate a random permutation of the elements of a list. 
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    gen <- getStdGen
    return (rnd_permu' gen xs (length xs) [])
    where
        rnd_permu' _ _ 0 acc = acc
        rnd_permu' gen xs n acc =
            let (randomIndex, newGen) = randomR (0, (length xs - 1)) gen
                (randomInit, x:randomTail) = splitAt randomIndex xs
            in rnd_permu' newGen (randomInit ++ randomTail) (n - 1) acc ++ [x]
    