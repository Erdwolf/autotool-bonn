module Shift.Computer where

-- -- $Id$

import Shift.Type
import Shift.Iterate
import Shift.Enum

import Data.FiniteMap
import Control.Monad ( guard, when, foldM )
import Random ( randomRIO )
import IO
import System
import Data.List (sort)

import Util.Zufall
import Util.Faktor

import Data.List ( group )

instance Show [Bool] where
    show = map ( \ x -> if x then '+' else '-' ) 

smp :: Show a => [a] -> IO ()
smp = sequence_ . map print

--------------------------------------------------------------


next0 :: Int -> Pins -> [Bool] -> [Bool]
-- wegen effizienz wird maximum ps nicht immer ausgerechnet
next0 m ps xs = 
    let this = not $ and $ do p <- ps ; return $ xs !! (p - 1) 
    in  this `seq` this : take (m-1) xs 

next :: Pins -> [Bool] -> [Bool]
next ps = next0 (maximum $ 0 : ps)  ps

strictly :: [[Bool]] -> [[Bool]]
-- das ist trivial wahr, erzwingt aber die auswertung der liste
strictly = takeWhile (( >= 0) . sum . map fromEnum )


zustands_folge :: Pins -> [[ Bool ]]
zustands_folge ps = 
    let m = maximum (0 : ps)
	start = replicate m True 
	fun = next0  m ps
    in  tail 
	     $ strictly
	     $ iterate_strict fun $ start

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



-- ffind :: Eq a => (a -> a) -> a -> Int
ffind :: ([Bool] -> [Bool]) -> [Bool] -> Int 

-- etwas more tricky:
-- vergleicht die folgen  f x0, f f x0, ..., f^k x0
--                   und  f f x0, f f f f x0, ..., f^{2k} x0
ffind next x0 =
    let slow = strictly $ iterate_strict next x0
	fast = strictly $ iterate_strict (\ x -> next $! next $! x ) x0

	-- c ist das kleinste c mit f^c x0 == f^{2c} x0
	x = head $ do
		   ( x ,y ) <- tail $ zip slow fast
		   guard $ x == y
		   return x

	-- bestimme das erste vorkommen von x danach 
	-- (ergibt kleinste periodenlänge)
	d = head $ do (i, y) <- tail $ zip [0..] 
		                     $ strictly $ iterate_strict next x
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


    