module Grammatik.CF.Epsfrei where

--  $Id$


import Grammatik.Type
import Grammatik.CF.Nullable

import Autolib.Set

import Control.Monad (guard)

-- | eingabe: kontextfreie Grammatik G1
-- ausgabe: kontextfreie Grammatik G2 mit L(G2) = L(G1) - Eps
-- und G2 enthält keine Regeln  V -> Eps
epsfrei :: Grammatik -> Grammatik
epsfrei g = let
    ns = nullable g
    rewrite "" = return ""
    rewrite (c : cs) = do 
        rest <- rewrite cs
	[ c : rest ] ++ [ rest | c `elementOf` ns ]
    neu = do 
	(lhs, rhs) <- rules g
	rhs' <- rewrite rhs
	guard $ not $ null rhs'
	return (lhs, rhs')
  in g { regeln = mkSet neu }

