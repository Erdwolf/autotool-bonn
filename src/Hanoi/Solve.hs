module Hanoi.Solve where

-- $Id$

import Hanoi.Type
import Data.Array

drei n von nach hilf = 
    if n > 0 
       then    drei (n-1) von hilf nach
	    ++ [ ( von, nach ) ]
	    ++ drei (n-1) hilf nach von
       else []


vier n von nach h1 h2 =
    if n > 0
       then let k = kah n
	    in     vier k     von h1   nach h2
		++ drei (n-k) von nach h2
		++ vier k     h1  nach von  h2
       else []

vau n = fst $ best ! n
kah n = snd $ best ! n
dee n = 2 ^ n - 1

best :: Array Integer (Integer, Integer)
best = array (1, 100) $ do 
--     n <- range $ bounds best
     n <- range (1, 100)
     let ( v, k ) = minimum $ ( dee n, 0 ) : do
	   k <- [ 1 .. n - 1 ]
	   let v = 2 * vau k + dee (n-k)
	   return ( v, k )
     return (n, ( v, k ))

