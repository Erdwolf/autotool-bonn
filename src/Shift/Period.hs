module Shift.Period where

import Shift.Type
import Shift.Computer
import Shift.Repeater

import Data.List ( group, inits, tails )
import Control.Monad ( guard )

ps k = [ 2*k, 3*k, 5*k+1, 7*k+1 ]

lift :: [ Int ] -> [ Bool ]
lift [] = []
lift ( neg : pos : rest ) =
    replicate neg False ++ replicate pos True ++ lift rest

-- Beispiel: show $ lift [5,24,1,7] 
--  = "-----++++++++++++++++++++++++-+++++++"

period k = 
    do l <- [ 0 .. k-1 ]
       ( if l == 0
         then [ 2*k, 3*k, 1, 4*k, k, 1, k-1, 2*k ]  
         else [ l, k-l, k, l, k-l, 2*k ]
         ) ++ do m <- [ 1 .. l ]
		 do d <- [ 0 .. 1 ]
		    [ m , k-m, m+d, 3*k, l-m+1-d, k-l+m ] 
		      ++ if m < l then [ k-m, l  , k-l,   2*k ] 
				  else [ k-m, l+d, k-l-d, 2*k ] 
           ++ do m <- [ l+1 .. k-1 ]
		 do d <- [ 0 .. 1 ]
		    [ l+1, k-l-1, m+d, k+l-m+1-d, m-l , 3*k 
		      , k-m, m+d, k-m-d, 2*k  ]


count :: Eq a => [a] -> [Int]
count = map length . group 

xs = count . folge . ps 

diffs xs ys = do
    t @ (k, x, y) <- zip3 [0..] xs ys
    guard $ x /= y
    return t
