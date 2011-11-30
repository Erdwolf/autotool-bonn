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


generate2 :: Int -> Int -> Int -> Int -> Int -> IO (String, String)
generate2 n d r i a = do
    s <- replicateM n (randomChar a)
    t <- insertRandom i a =<< replaceRandom r a =<< deleteRandom d s
    return (s,t)

deleteRandom 0 xs = return xs
deleteRandom d xs = do
    k <- randomRIO (0,length xs-1)
    let (xs1,(_:xs2)) = splitAt k xs
    deleteRandom (pred d) (xs1++xs2)

replaceRandom 0 _ xs = return xs
replaceRandom r a xs = do
    k <- randomRIO (0,length xs-1)
    let (xs1,(_:xs2)) = splitAt k xs
    x <- randomChar a
    replaceRandom (pred r) a (xs1++(x:xs2))

insertRandom 0 _ xs = return xs
insertRandom i a xs = do
    k <- randomRIO (0,length xs)
    let (xs1,xs2) = splitAt k xs
    x <- randomChar a
    insertRandom (pred i) a (xs1++(x:xs2))
