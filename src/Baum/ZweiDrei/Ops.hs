module Baum.ZweiDrei.Ops where

--  $Id$

import Baum.ZweiDrei.Type 

zwei :: Int
zwei = 2

drei :: Int
drei = 3

contains :: Ord a
	 => Baum a -> a -> Bool
contains Null         x = False
contains ( Baum bks ) x = 
    let handle [] = False
	handle ( (b, k) : bks ) =
	    if This x == k then True
	    else if This x < k then contains b x
		 else handle bks
    in  handle bks 

contents :: Baum a -> [ a ] 
contents Null = []
contents ( Baum bks ) = do
    (b, k) <- bks
    contents b ++ [ x | This x <- [ k ] ]

--------------------------------------------------------------------------

insert :: Ord a => Baum a -> a -> Baum a
insert b x = 
    case split_insert ( b, Infinity ) x of
        [ ( b', Infinity ) ] -> b'
	bks -> Baum bks
    
list_insert :: Ord a 
	    => [ (Baum a, Key a) ] 
	    -> a
	    -> [ (Baum a, Key a) ] 
list_insert bks @ ((b, k) : rest) x = 
    if This x < k
    then split_insert (b, k) x ++ rest
    else (b, k) : list_insert rest x

split_insert :: Ord a 
	     => ( Baum a, Key a ) 
	     -> a 
	     ->  [ (Baum a, Key a) ] 
split_insert ( Null, k ) x =
     [ (Null, This x), ( Null, k ) ]
split_insert ( Baum bks, k ) x =
    let bks' = list_insert bks x
    in  if length bks' <= drei
        then [ ( Baum bks', k ) ]
        else let ( pre, post ) = splitAt (length bks' `div` 2 ) bks'
                 ( last_b, last_k ) = last pre
		 pre' = init pre ++ [ ( last_b, Infinity ) ]
	     in  [ ( Baum pre', last_k), ( Baum post, k ) ]

