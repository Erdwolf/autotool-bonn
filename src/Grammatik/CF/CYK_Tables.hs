
module Grammatik.CYK_Tables 

-- $Id$

( module Simple_Set

, ctable
, vtable
, table_lookup
)

where

-- $Log$
-- Revision 1.1  2003-11-25 08:21:09  joe
-- moved CF-related files into subdir
--
-- Revision 1.1  2002/12/17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1.1.1  2002/05/24 10:46:47  challenger
-- start
--
-- Revision 1.2  2001/11/29 13:41:20  autotool
-- CF_advanced: besserer parser, härtere tests
--
-- Revision 1.1  2001/11/28 07:27:11  autotool
-- benutze DPL in CYK/CF_Chomsky
--

import  Grammatik.Chomsky

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




    