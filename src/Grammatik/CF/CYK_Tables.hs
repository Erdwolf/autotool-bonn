module Grammatik.CF.CYK_Tables 

-- -- $Id$

( module Autolib.Simple_Set

, ctable
, vtable
, table_lookup
)

where


import  Grammatik.CF.Chomsky

import Autolib.FiniteMap
import Autolib.Simple_Set
-- import Data.Set

import Data.Array

table_lookup fm = lookupWithDefaultFM fm emptySet

-- | tafel für characters (terminale)
ctable :: Ord a => Rules a -> FiniteMap Char (Set a)
ctable rules = addListToFM_C union emptyFM $ do
    ( lhs, Left c ) <- rules
    return ( c, unitSet lhs )

vtable :: Ord a => Rules a -> FiniteMap (a, a) (Set a)
-- tafel für paare von variablen
vtable rules = addListToFM_C union emptyFM $ do
    ( lhs, Right (x, y) ) <- rules
    return ( (x, y), unitSet lhs )




    