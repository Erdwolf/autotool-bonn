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

merge :: [Int] -> [Int] -> [(Int,Int)]
merge [] ys = []
merge xs [] = []
merge [x] [y] = [(x,y)]
merge xs ys = 
    let (xodds, xevens) = halve xs
	(yodds, yevens) = halve ys
    in     merge xodds yodds
	++ merge xevens yevens
        ++ zip (xevens ++ yevens)  (tail xodds ++ yodds)

halve [] = ([], [])
halve (x : xs) = 
    let ( here, there ) = halve xs
    in  ( x : there, here )

halve' xs = let ( here, there ) = halve $ reverse xs
	    in ( reverse there, reverse here )

