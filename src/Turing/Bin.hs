module Turing.Bin where

--   $Id$

bin :: Int -> [ Int ]
bin n = reverse $ bins n
    where bins 0 = []
	  bins n = let (a,b) = divMod n 2 
		   in b : bins a
	  
binstr :: Int -> String
binstr 0 = "0" -- gotcha!
binstr n = foldl1 (++) $ map show $ bin n

unstr :: Int -> String
unstr n = take n $ repeat '1'
