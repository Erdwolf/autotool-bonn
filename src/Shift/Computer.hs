module Shift.Computer where

-- $Id$

import Shift.Type

import FiniteMap
import Monad ( guard, when, foldM )
import Random
import IO
import System
import List (sort)

import Util.Zufall


zustands_folge :: Pins -> [[ Bool ]]
zustands_folge ps = 
    let m = maximum (0 : ps)
	start = replicate m True 
	next :: [Bool] -> [Bool]
	next xs = take m
		$ not ( and $ do p <- ps ; return $ xs !! (p - 1) )
		: xs
    in  tail $ iterate next $ start

folge :: Pins -> [ Bool ]
folge = map head . zustands_folge

find :: Ord a => [a] -> (Int, Int)
-- findet Länge von Vorperiode und Periode 
-- für eine unendliche schließlich periodische Folge
find xs = 
    let handle cache ((k,x) : rest) = 
	       case lookupFM cache x of
		    Just i -> ( i, k-i )
		    Nothing -> handle ( addToFM cache x k ) rest
    in	handle emptyFM $ zip [0..] xs

some :: Int -> IO Pins
some n = do 
    xfs <- mapM ( \ x -> do f <- randomRIO (1, n) ; return (x, f ) )
		[ 1 .. n ]
    return $ do (x, f) <- xfs ; guard $ f*f < n ; return x

somes :: Int -> Int -> IO Pins
somes n k = do
    ps <- sequence $ do i <- [ 1 .. k ] ; return $ randomRIO (1, n)
    return $ sort ps

suche :: Int -> Int -> IO () 
suche n k = 
    let handle top = do
	    ps <- somes n k
	    let (q, p) = find $ zustands_folge ps
	    when ( p  > top ) $ print ( ps, q, p )	
	    when ( p == top ) $ putStr " * " 
	    hFlush stdout
	    handle (max top p)
    in	handle 0


    