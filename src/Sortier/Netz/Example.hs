module Sortier.Netz.Example where

-- -- $Id$

import Sortier.Netz.Type

bubble :: Int -> Netz
bubble n = mkNetz $ do
    hi <- [ 2 .. n ]
    lo <- reverse [ 1 .. pred hi ]
    return ( lo, succ lo )

bad_example :: Int -> Netz
bad_example n =  mkNetz $ do
    hi <- [ 2 .. n ]
    lo <- reverse [ 1 , 3 .. pred hi ]
    return ( lo, succ lo )
