-- -- $Id$

module NPDA.Dot

( module Dot.Dot
)

where

import NPDA.Type

import Dot.Dot
import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge

import ToDoc
import Set
import FiniteMap
import Maybe

-- zust�nde werden mit [0 .. ] durchnumeriert
-- akzeptierende zust�nde bekommen doppelkreis drumrum
-- startzust�nde bekommen pfeil dran,
-- dieser kommt aus unsichtbarem zustand mit idents U0, U1, ..

instance NPDAC Char Char z
	 => ToDot ( NPDA Char Char z ) where
    toDot a = 
        let fm = listToFM $ zip (setToList $ zustandsmenge a) $ [ 0.. ] 
	    num = fromMaybe (error "NPDA.Dot.num") . lookupFM fm

	    -- tats�chliche knoten (zust�nde)
	    ns = do let finals = case akzeptiert a of
			       Leerer_Keller -> emptySet
			       Zustand xs    -> xs
		    p <- setToList $ zustandsmenge a
		    let sh = case p `elementOf` finals of
			      True  -> "doublecircle"
			      False -> "circle"
		    return $ Dot.Node.blank
			   { Dot.Node.ident = show $ num p
			   , Dot.Node.label = Just $ render $ toDoc p
			   , Dot.Node.shape = Just sh
			   }

	    -- unsichtbare knoten (f�r start-pfeile)
	    uns = do let p = startzustand a
		     return $ Dot.Node.blank
			   { Dot.Node.ident = "U" ++ show ( num p )
			   , Dot.Node.node_style = Just "invis"
			   }
    
	    -- tats�chliche zustands�berg�nge
	    es = do ( (mx, z, y), (z', ys') ) <- unCollect $ tafel a
		    let form "" = "Eps" ; form cs = cs
			lab =  "(" ++ form ( maybeToList mx )
			    ++ "," ++ [ y ]
			    ++ "," ++ form ys'
			    ++ ")"			    
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = show $ num z
			   , Dot.Edge.to    = show $ num z'
			   , Dot.Edge.taillabel = Just $ show lab
			   }
	    -- start-pfeile
	    ss = do p <- [ startzustand a ]
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = "U" ++ show ( num p )
			   , Dot.Edge.to    = show $ num p
			   }

	in  Dot.Graph.Type 
	    { Dot.Graph.directed = True
	    , Dot.Graph.name = "NPDA"
	    , Dot.Graph.nodes = ns ++ uns
	    , Dot.Graph.edges = es ++ ss
	    }


