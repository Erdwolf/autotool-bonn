module Shift.Analyze where

-- $Id$

import Shift.Type
import Shift.Computer
import Shift.Repeater
import Shift.Common

import Monad ( guard )

import FiniteMap
import Maybe

type State = [ Bool ]

instance Show State where
   show xs = do x <- xs ; if x then "+" else "-"

type STI = (State, Int)

hamming :: State -> State -> Int
-- anzahl verschiedener positionen
hamming xs ys = length $ do 
	(x,y) <- zip xs ys
	guard $ x /= y
	return ()

weight :: State -> Int
-- anzahl der True-Positionen
weight xs = length $ do
       x <- xs
       guard x
       return ()

analyze :: Pins -> [ (STI, STI) ]
analyze ps =
    let start = (replicate (maximum ps) True, 0)
    in  helper ( next ps )  
	       ( listToFM [ ( weight $ fst start, [start] ) ] )
	       start

helper :: (State -> State) -> FiniteMap Int [ STI ] -> STI
       -> [(STI, STI)] 
helper next fm xi @ (x, i) =
    let y = next x
	w = weight y
	yi = (y, i+1)
	cs = do v <- [w-1 .. w+1]
		(z, k) <- concat $ maybeToList $ lookupFM fm v
		guard $ 1 >= hamming z y 
		return (z, k)
	ds = do (z, k) <- cs ; guard $ z == y ; return ( z, k )
	fm' = addToFM_C (++) fm w [yi]
    in	do zk <- cs ; return ( yi, zk )
	++ if null ds then helper next fm' yi else []

