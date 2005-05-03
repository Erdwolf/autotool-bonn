module Partition.Roll where

-- | (c) M. Rahn,  see Garey/Johnson

-- | PARTITION_INT: 
-- | Input: Set A of numbers.
-- | Question: Exists A' \subseteq A with sum(A')=sum(A\A')?

-- $Id$

import Control.Monad ( when )
import Control.Monad.ST ( ST , runST )
import Data.Array.ST ( STUArray , newArray , readArray , writeArray )
import Data.List ( nub )
import System.Random ( randomRIO )

-------------------------------------------------------------------------------

-- | eigentlich MultiSet
type P = [ Int ]

-- | random number sequence, distinct numbers, solvable
rand :: Int -> Int -> Int -> IO P
rand l i m = rand_solvableP l i m >>= \ p ->
	     if nub p == p then return p else rand l i m

-- | random number sequence, solvable
rand_solvableP :: Int -> Int -> Int -> IO P
rand_solvableP l i m = randP l i m >>= \ p -> 
		       if solvable p then return p else rand_solvableP l i m

-- | random number sequence
-- first arg: length
-- second arg: min bound
-- third arg: max bound
randP :: Int -> Int -> Int -> IO P
randP l i = sequence . replicate l . randomRIO . (,) i

-------------------------------------------------------------------------------
-- | algorithm: Garey/Johson, computers and intractability, sec 42., p. 90

type A s = STUArray s (Int,Int) Bool

solvable :: P -> Bool
solvable []  = False
solvable [x] = x == 0
solvable p
    | not $ even b = False
    | otherwise    = runST (dynprog p (div b 2) (length p))
    where b = sum p

dynprog :: P -> Int -> Int -> ST s Bool
dynprog [] _ _ = return False
dynprog p h n = do
	  a <- newArray ((1,0),(n,h)) False :: ST s ( A s )
	  mapM ( \ j -> writeArray a (1,j) $ or [ j==0 , j==(head p) ] ) [0..h]
	  mapM ( \ ((i,v),j) -> do
		 x <- readArray a (i-1,j)
                 y <- if v <= j then readArray a (i-1,j-v) else return False
		 when ( or [ x , y ] ) $ writeArray a (i,j) True
	       ) $ do i <- zip [2..n] (tail p); j <- [0..h] ; return (i,j)
	  readArray a (n,h)
