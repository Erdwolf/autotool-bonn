module Grammatik.Epsfrei where

-- $Id$


import Grammatik.Type

import Fix
import Set

import Monad (guard)

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
-- Revision 1.1  2001/11/18 23:57:05  autotool
-- neu: chomsky-form, greibach-form, CYK-parser und hilfsprogramme
--

epsfrei :: Grammatik -> Grammatik
-- eingabe: kontextfreie Grammatik G1
-- ausgabe: kontextfreie Grammatik G2 mit L(G2) = L(G1) - Eps
-- und G2 enth�lt keine Regeln  V -> Eps
epsfrei g = let
    nullable = fix ( \ ns -> mkSet $ do
        ( [ lhs ] , rhs ) <- rules g
	guard $ and [ x `elementOf` ns | x <- rhs ]
	return lhs ) emptySet
    rewrite "" = return ""
    rewrite (c : cs) = do 
        rest <- rewrite cs
	[ c : rest ] ++ [ rest | c `elementOf` nullable ]
    neu = do 
	(lhs, rhs) <- rules g
	rhs' <- rewrite rhs
	guard $ not $ null rhs'
	return (lhs, rhs')
  in g { regeln = mkSet neu }

