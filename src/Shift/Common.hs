module Shift.Common 

where

import Control.Monad ( guard )
import Data.List (inits, tails)
import Data.FiniteMap

-- common :: Eq a => [a] -> [(Int, Int, Int)]
common xs = do
    (i, ys) <- zip [0..] $ tails xs
    (j, zs) <- tail $ zip [i..] $ tails ys
    let w = com ys zs 
    let l = length w
    guard $  l > 0
    return (l, w, i, j)

com (x : xs) (y : ys) | x == y = x : com xs ys
com _ _ = []

up :: Ord a => [a] -> [a]
up [] = []
up (x : xs) = x : up (filter (> x) xs)


oft :: Ord a => [a] -> FiniteMap [a] [Int]
oft xs = addListToFM_C (++) emptyFM $ do
    ys <- inits xs
    (k, zs) <- zip [0..] $ tails ys
    return (zs, [k])
    
