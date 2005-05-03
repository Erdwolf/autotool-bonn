module Partition.Roll where

-- | (c) M. Rahn,  see Garey/Johnson

-- | PARTITION_INT: 
-- | Input: Set A of numbers.
-- | Question: Exists A' \subseteq A with sum(A')=sum(A\A')?

-- $Id$

import Control.Monad ( when , guard )
import Control.Monad.ST ( ST , runST )
import Data.Array.ST ( STUArray , newArray , readArray , writeArray , freeze )
import Data.Array ( Array , (!) )
import Data.List ( nub , (\\) , sort )
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
    | otherwise    = runST (dynprog readArray p (div b 2) (length p))
    where b = sum p

dynprog :: ( A s -> (Int,Int) -> ST s a ) -> P -> Int -> Int -> ST s a
dynprog f p h n = do
	  a <- newArray ((1,0),(n,h)) False :: ST s ( A s )
	  mapM ( \ j -> writeArray a (1,j) $ or [ j==0 , j==(head p) ] ) [0..h]
	  mapM ( \ ((i,v),j) -> do
		 x <- readArray a (i-1,j)
                 y <- if v <= j then readArray a (i-1,j-v) else return False
		 when ( or [ x , y ] ) $ writeArray a (i,j) True
	       ) $ do i <- zip [2..n] (tail p); j <- [0..h] ; return (i,j)
	  f a (n,h)

-------------------------------------------------------------------------------
-- | find a solution: traverse the table for an True->True->... way

type Arr = Array (Int,Int) Bool

construct :: P -> Int -> Int -> ST s Arr
construct = dynprog ( \ a _ -> freeze a )

-- | PRECONDITION: solvable p is NOT checked
solve :: P -> [(P,P)]
solve p = let b = sum p
	      h = div b 2
	      n = length p
	      a = runST (construct p h n)
	      s = do path <- map ( nub . map snd ) $ find a p (n,h)
		     guard $ last path == 0
		     let l = zipWith (-) path (tail path)
			 r = p \\ l
		     guard $ sum l == sum r
		     return (l,r)
          in s

find :: Arr -> P -> (Int,Int) -> [[(Int,Int)]]
find _ _ (_,0) = [[]]
find a p (1,j) = do v <- take 1 p
		    guard $ v <= j
		    guard $ a ! (1,j-v)
		    return [(1,j),(1,j-v)]
find a p (i,j) = do d <- concatMap (find a p) $ do 
			 v <- 0 : take i p
			 guard $ v <= j
			 guard $ a ! (i-1,j-v)
			 return (i-1,j-v)
		    return $ (i,j) : d

verify :: P -> (P,P) -> Bool
verify p (l,r) = and [ sum l == sum r
		     , sort ( l ++ r ) == sort p
		     ]
