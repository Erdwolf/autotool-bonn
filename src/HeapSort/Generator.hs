module HeapSort.Generator where

import System.Random (randomR, getStdGen, setStdGen)
import Data.List (nubBy)
import Data.Function (on)

generate :: Int -> (Int, Int) -> IO [Int]
generate n range = do
    g <- getStdGen
    let xs = take n $ nubBy ((==)`on`fst) $ randomRs' range g
    setStdGen $ snd $ last xs
    return (map fst xs)

--randomRs' :: RandomGen g => (a,a) -> g -> [(a,g)]
randomRs' ival g = (x,g') : randomRs' ival g' where (x,g') = randomR ival g
