module HeapSort.Generator where

import System.Random

generate :: IO [Int]
generate = do
    let n = 7; range = (1,100)
    g <- getStdGen
    return $ take n $ nub $ randomRs range g
