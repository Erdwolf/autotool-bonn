module Grammatik.CF.CYK_Tables 

-- -- $Id$

( module Simple_Set

, ctable
, vtable
, table_lookup
)

where


import  Grammatik.CF.Chomsky

import FiniteMap
import Simple_Set
-- import Set

import Array

table_lookup fm = lookupWithDefaultFM fm emptySet

ctable :: Ord a => Rules a -> FiniteMap Char (Set a)
-- tafel für characters (terminale)
ctable rules = addListToFM_C union emptyFM $ do
    ( lhs, Left c ) <- rules
    return ( c, unitSet lhs )

vtable :: Ord a => Rules a -> FiniteMap (a, a) (Set a)
-- tafel für paare von variablen
vtable rules = addListToFM_C union emptyFM $ do
    ( lhs, Right (x, y) ) <- rules
    return ( (x, y), unitSet lhs )




    