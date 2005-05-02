module Partition.Roll where

-- | (c) M. Rahn,  see Garey/Johnson

-- | PARTITION_INT: 
-- | Input: Set A of numbers.
-- | Question: Exists A' \subseteq A with sum(A')=sum(A\A')?

import Control.Monad ( when )
import Control.Monad.ST ( ST , runST )
import Data.Array.ST ( STUArray , newArray , readArray , writeArray )
import Data.List ( nub )
import System.Random ( randomRIO )

-------------------------------------------------------------------------------

-- | eigentlich MultiSet
type P = [ Int ]

-- | random number sequence, distinct numbers, solvable
rand :: Int -> Int -> IO P
rand l m = rand_solvableP l m >>= \ p ->
	   if nub p == p then return p else rand l m

-- | random number sequence, solvable
rand_solvableP :: Int -> Int -> IO P
rand_solvableP l m = randP l m >>= \ p -> 
		     if solvable p then return p else rand_solvableP l m

-- | random number sequence
-- first arg: length
-- second arg: max bound
-- (min bound is fixed to 1)
randP :: Int -> Int -> IO P
randP l = sequence . replicate l . randomRIO . (,) 1

-------------------------------------------------------------------------------
-- | algorithm: Garey/Johson, computers and intractability, sec 42., p. 90

type A s = STUArray s (Int,Int) Bool

solvable :: P -> Bool
solvable []  = False
solvable [0] = True
solvable [_] = False
solvable p
    | not $ even b = False
    | otherwise    = runST (dynprog p (div b 2) (length p))
    where b = sum p

dynprog :: P -> Int -> Int -> ST s Bool
dynprog p h n = do
	  a <- newArray ((1,0),(n,h)) False :: ST s ( A s )
	  mapM ( \ j -> writeArray a (1,j) $ or [ j==0 , j==p!!0 ] ) [0..h]
	  mapM ( \ (i,j) -> do
		 x <- readArray a (i-1,j)
                 y <- if p!!(pred i) <= j then readArray a (i-1,j-p!!(pred i))
		                          else return False
		 when ( or [ x , y ] ) $ writeArray a (i,j) True
	       ) $ do i <- [2..n] ; j <- [0..h] ; return (i,j)
	  readArray a (n,h)
