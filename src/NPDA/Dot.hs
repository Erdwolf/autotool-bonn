--   $Id$

module NPDA.Dot

( module Autolib.Dot.Dot
)

where

import NPDA.Type

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph as G
import qualified Autolib.Dot.Node as N
import qualified Autolib.Dot.Edge as E

import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import Data.Maybe

-- | zust�nde werden mit [0 .. ] durchnumeriert
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
		    return $ N.blank
			   { N.ident = show $ num p
			   , N.label = Just $ render $ toDoc p
			   , N.shape = Just sh
			   }

	    -- unsichtbare knoten (f�r start-pfeile)
	    uns = do let p = startzustand a
		     return $ N.blank
			   { N.ident = "U" ++ show ( num p )
			   , N.node_style = Just "invis"
			   }
    
	    -- tats�chliche zustands�berg�nge
	    es = do ( mx, z, y, z', ys' ) <- unCollect' $ transitionen a
		    let form "" = "Eps" ; form cs = cs
			lab =  "(" ++ form ( maybeToList mx )
			    ++ "," ++ [ y ]
			    ++ "," ++ form ys'
			    ++ ")"			    
		    return $ E.blank
			   { E.from  = show $ num z
			   , E.to    = show $ num z'
			   , E.taillabel = Just $ show lab
			   }
	    -- start-pfeile
	    ss = do p <- [ startzustand a ]
		    return $ E.blank
			   { E.from  = "U" ++ show ( num p )
			   , E.to    = show $ num p
			   }

	in  G.Type 
	    { G.directed = True
	    , G.name = "NPDA"
	    , G.nodes = ns ++ uns
	    , G.edges = es ++ ss
	    , G.attributes = []
	    }


