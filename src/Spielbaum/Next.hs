module Spielbaum.Next where

-- this file is copied only (source: /autotool/autobahn/Next.hs)
-- reason for copying: avoid imports from other directories

import Graph.Graph
import Schichten
import Schichten

import Graph.Type
import Graph.Viz

import Data.Set
import Data.FiniteMap
import Maybe

----------------------------------------------------------------------------

class Next a where 
      next :: a -> [ a ]

spielbaum :: (Ord a, Next a) 
	  => a -> Graph a
spielbaum w = 
    let xs = unionManySets $ schichten ( mkSet . next ) w
	ks = mkSet $ do 
	        x <- setToList $ xs
		y <- next x
		return $ Kante { von = x, nach = y }
    in	Graph { knoten = xs
	      , kanten = ks 
	      }

----------------------------------------------------------------------------

paint :: (Show a, ShowText a, Next a, Ord a) 
      => String -> a ->  IO ()
paint fname w = do
    let g = spielbaum w
	-- knoten durchnumerieren
	fm = listToFM $ zip (setToList $ knoten g) [ 1 .. ]
	t = fromMaybe (error "Next.paint.t") . lookupFM fm
    getGraphviz (spielbaum w) (instanzTrans t) fname
    return ()

instanzTrans :: ShowText knoten 
	     => (knoten -> Int) -> GVTrans knoten
instanzTrans t = GVTrans
	{ getGVProg = Default
	, getGVFormat = "ps"
	, isGVDirected = True
	, getGVNID = show . t
	, getGVNName = showText -- Knotenname auch
	, getGVNLabel = Nothing
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}
