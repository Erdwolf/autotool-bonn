module HeapSort.Generator where

import System.Random (randomR, getStdGen, setStdGen, randomRIO)
import Data.List (nubBy, sort, groupBy, unfoldr)
import Data.Function (on)


generate :: Int -> Int -> (Int, Int) -> IO [Int]
generate k n range = do
    xs <- uniqueRandomRsIO n range
    let xss = hackIntoPieces k n (sort xs)
    let xss' = reverse (take ((k+1)`div`2) xss) ++ drop ((k+1)`div`2) xss
    xss'' <- mapM randomPermIO xss'
    return (concat xss'')


hackIntoPieces :: Int -> Int -> [a] -> [[a]]
hackIntoPieces k n = map (map snd) . groupBy (p `on` fst) . zip [0..]
    where p _ j = j `mod` (n`div`k) /= 0 || j >= n-(n`mod`k)

uniqueRandomRsIO :: Int -> (Int, Int) -> IO [Int]
uniqueRandomRsIO n range = do
    g <- getStdGen
    let xs = take n $ nubBy ((==)`on`fst) $ randomRs' range g
    setStdGen $ snd $ last xs
    return (map fst xs)

--randomRs' :: RandomGen g => (a,a) -> g -> [(a,g)]
randomRs' ival g = (x,g') : randomRs' ival g' where (x,g') = randomR ival g

randomPermIO :: [a] -> IO [a]
randomPermIO xs = do
   let n = length xs
   is <- sequence [randomRIO (0,k) | k <- [n-1,n-2..0]]
   return $ unfoldr uf (is,xs)
 where
   uf ([],_) = Nothing
   uf (i:is,xs) =
      let (xs1,x:xs2) = splitAt i xs
      in Just (x, (is,xs1++xs2))

