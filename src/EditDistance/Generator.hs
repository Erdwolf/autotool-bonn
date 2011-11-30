module EditDistance.Generator where

import System.Random (randomRIO)
import Control.Monad (replicateM)


generate :: Int -> Int -> Int -> IO (String, String)
generate n m a = do
    s <- replicateM n (randomChar a)
    t <- replicateM m (randomChar a)
    return (s,t)


randomChar a = do
    i <- randomRIO (0,a-1)
    return (['a'..] !! i)

