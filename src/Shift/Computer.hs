module Shift.Computer where

-- $Id$

import Shift.Type
import Shift.Iterate
import Shift.Enum

import FiniteMap
import Monad ( guard, when, foldM )
import Random ( randomRIO )
import IO
import System
import List (sort)

import Util.Zufall
import Util.Faktor

instance Show [Bool] where
    show = map ( \ x -> if x then '+' else '-' ) 

smp :: Show a => [a] -> IO ()
smp = sequence_ . map print

--------------------------------------------------------------


next0 :: Int -> Pins -> [Bool] -> [Bool]
-- wegen effizienz wird maximum ps nicht immer ausgerechnet
next0 m ps xs = take m
		$ not ( and $ do p <- ps ; return $ xs !! (p - 1) )
		: xs

next :: Pins -> [Bool] -> [Bool]
next ps = next0 (maximum $ 0 : ps) ps

zustands_folge :: Pins -> [[ Bool ]]
zustands_folge ps = 
    let m = maximum (0 : ps)
	start = replicate m True 
    in  tail $ iterate_strict (next0 m ps) $ start

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

-------------------------------------------------------------------




-------------------------------------------------------------------


{-# SPECIALIZE find :: ([Bool] -> [Bool]) -> [Bool] -> Int #-}

ffind :: Eq a => (a -> a) -> a -> Int
-- etwas more tricky:
-- vergleicht die folgen  f x0, f f x0, ..., f^k x0
--                   und  f f x0, f f f f x0, ..., f^{2k} x0
ffind next x0 =
    let slow = iterate_strict next x0
	fast = iterate_strict (next . next) x0
	link = tail $ zip3 [0..] slow fast

	-- c ist das kleinste c mit f^c x0 == f^{2c} x0
	( c, x, y ) = head $ filter ( \ (c,x,y) -> x == y ) $ link

	-- bestimme das erste vorkommen von x danach 
	-- (ergibt kleinste periodenlänge)
	d = head $ do (i, y) <- tail $ zip [0..] $ iterate_strict next x
		      guard $ y == x
		      return i
    in	d

--------------------------------------------------------------------------

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
suche n k = do
    putStrLn $ "optimiere q"

    let handle top ( ps : rest ) = do

	    let s = maximum ( 0 : ps )
	    let p = ffind (next0 s ps) $ replicate s True
	    let q = -1

	    -- hiernach wird optimiert
	    let m = p

	    let sh = Shift { pins = ps, vorperiode = q, periode =  p }	
	    when ( m  > top ) $ print (sh, primfaktoren $ fromIntegral p)
	    when ( m == top ) $ putStr " * " 
	    handle ( max m top ) rest

    handle 0 $ subsets k [1 .. n]


    