module Sortier.Netz.OE where

--  $Id$

sort :: [ Int ] -> [(Int,Int)]
sort xs = 
    if length xs <= 1
    then []
    else let ( bot, top ) = splitAt (succ (length xs) `div` 2) xs
	 in    sort bot
	    ++ sort top
            ++ merge bot top

-- merge [x1, x2, .. xj ] [ y1, y2, .. yk ]
-- = let z  = merge [ x1 x3 .. ] [ y1 y3 .. ]
--       z' = merge [ x2 x4 .. ] [ y2 y4 .. ]
--   in  z1 (swap z1' z2) (swap z2' z3) .. 

-- merge        [1 3 6 7 9] [2 4 5 ]
--   z  = merge [1   6   9] [2   5 ] = 1   2   5 6   9
--   z' = merge [  3   7  ] [  4   ] =   3   4     7
--     [ 1, 2 <-> 3, 5 <-> 4, 6 <-> 7, 9

-- merge [ 1 5 ] [ 2 3 6 7 8 ]
--  merge[ 1   ] [ 2   6   8 ] => [ 1    2   6   8 ]
-- merge [   5 ] [   3   7   ] => [    3   5   7   ]

merge :: [Int] -> [Int] -> [(Int,Int)]
merge [] ys = []
merge xs [] = []
merge [x] [y] = [(x,y)]
merge xs ys = 
    let (xodds, xevens) = halve xs
	(yodds, yevens) = halve ys
    in     merge xodds yodds
	++ merge xevens yevens
        ++ if odd (length xs)
	   then zip (xevens ++ yodds) (tail xodds ++ yevens)
	   else zip (xevens ++ yevens) (tail xodds ++ yodds)

halve [] = ([], [])
halve (x : xs) = 
    let ( here, there ) = halve xs
    in  ( x : there, here )

halve' xs = let ( here, there ) = halve $ reverse xs
	    in ( reverse there, reverse here )

