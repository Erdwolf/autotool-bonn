module Grammatik.CF.Kettenfrei where

-- -- $Id$

import Grammatik.Type

import Autolib.Fix
import Autolib.Set
import Autolib.FiniteMap
import qualified Autolib.Relation as Relation

import Data.List (partition)
import Control.Monad (guard)

-- | eingabe: kontextfreie Grammatik G1
-- ausgabe: kontextfreie Grammatik G2 mit L(G2) = L(G1)
-- und G2 enthält keine Regeln  V -> V
kettenfrei :: Grammatik -> Grammatik
kettenfrei g = let
    ( pairs, nopairs ) = partition ( \ ( [lhs], rhs) -> 
            length rhs == 1 && head rhs `elementOf` nichtterminale g ) 
        ( rules g )

    chains = Relation.trans 
	   $ Relation.plus ( Relation.identic $ mkSet $ vars g )
	                   ( Relation.make $ do 
			        ([l],[r]) <- pairs
			        return (l, r)
			   )

    rewrite "" = return ""
    rewrite (c : cs) = do 
        rest <- rewrite cs
	[ c : rest ] ++ 
	  [ rhs ++ rest 
	  | let ds = Relation.images chains c
	  , ( [ d ], rhs ) <- nopairs
	  , d `elementOf` ds
	  ]
    neu = do 
	(lhs, rhs) <- pairs 
	rhs' <- rewrite rhs
	guard $ rhs' /= rhs
	return (lhs, rhs')
  in g { regeln = mkSet $ neu ++ nopairs }


