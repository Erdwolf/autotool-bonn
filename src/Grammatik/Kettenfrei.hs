
module Grammatik.Kettenfrei where

-- $Id$

import Grammatik.Type

import Fix
import Set
import FiniteMap
import Relation

import List (partition)
import Monad (guard)

-- $Log$
-- Revision 1.1  2002-12-17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1.1.1  2002/05/24 10:46:47  challenger
-- start
--
-- Revision 1.1  2001/11/18 23:57:05  autotool
-- neu: chomsky-form, greibach-form, CYK-parser und hilfsprogramme
--


kettenfrei :: Grammatik -> Grammatik
-- eingabe: kontextfreie Grammatik G1
-- ausgabe: kontextfreie Grammatik G2 mit L(G2) = L(G1)
-- und G2 enthält keine Regeln  V -> V
kettenfrei g = let
    ( pairs, nopairs ) = partition ( \ ( [lhs], rhs) -> 
            length rhs == 1 && head rhs `elementOf` nichtterminale g ) 
        ( rules g )
    lookupset fm = lookupWithDefaultFM fm emptySet
    chains = trans $ addListToFM_C union 
		   ( listToFM [ (x, unitSet x) | x <- vars g ] )
		   [ (l, unitSet r) | ([l],[r]) <- pairs ]

    rewrite "" = return ""
    rewrite (c : cs) = do 
        rest <- rewrite cs
	[ c : rest ] ++ 
	  [ rhs ++ rest 
	  | let ds = lookupset chains c
	  , ( [ d ], rhs ) <- nopairs
	  , d `elementOf` ds
	  ]
    neu = do 
	(lhs, rhs) <- pairs 
	rhs' <- rewrite rhs
	guard $ rhs' /= rhs
	return (lhs, rhs')
  in g { regeln = mkSet $ neu ++ nopairs }


