module Sortier.Netz.Example where

-- $Id$

import Sortier.Netz.Type

bubble :: Int -> Netz
bubble n = mkNetz $ do
    hi <- [ 2 .. n ]
    lo <- reverse [ 1 .. pred hi ]
    return ( lo, succ lo )

