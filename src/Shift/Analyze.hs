module Shift.Analyze where

-- $Id$

import Shift.Type
import Shift.Computer
import Shift.Repeater
import Shift.Common

import Util.Uniq

import Monad ( guard )


import FiniteMap
import Set
import Maybe
import List ( inits, tails, group )

type State = [ Bool ]

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


subwords :: Int -> [a] -> [[a]]
subwords l xs = do
    ys <- tails xs
    let zs = take l ys
    guard $ length zs == l
    return zs

allwords :: Int -> [a] -> [[a]]
allwords 0 alpha = [[]]
allwords l alpha = do
    w <- allwords (l-1) alpha
    x <- alpha
    return $ x : w

defect :: Pins -> Int -> Int -> [[Bool]]
defect ps cut d = setToList $
    minusSet ( mkSet $ allwords d [ False, True ] )
	     ( mkSet $ subwords d $ take cut $ folge ps )

factor :: Eq a => [a] -> [a] -> Bool
factor u v = or $ do
    w <- tails v
    return $ take (length u) w == u

defects :: Int -> Pins -> [[[Bool]]]
defects cut ps = weed factor $ do d <- [1 .. ] ; return $ defect ps cut d

weed :: (b -> b -> Bool) -> [[b]] -> [[b]]
weed rel (xs : xss) = ( xs : ) $ weed rel $ do
    let fresh y = not $ or $ do x <- xs ; return $ rel x y
    ys <- xss
    return $ filter fresh ys

	   
    