module Faktor.Certify where

--  $Id$

powmod :: Integral b 
       => b -> b -> b -> b
powmod x 0 m = 1
powmod x n m = 
    let (q, r) = divMod n 2
        h = powmod x q m
	hh = (h * h) `mod` m
    in  if 0 == r 
	then hh
	else (hh * x) `mod` m
