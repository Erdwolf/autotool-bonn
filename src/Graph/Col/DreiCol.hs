-- | drei-färbung (mit würfeln) 
-- autor m.lindemeyer
-- stinfwww.informatik.uni-leipzig.de/~psy99hvr
-- (8484955)
-- patches: joe@informatik.uni-leipzig.de

module Col.DreiCol
	( DreiCol, Faerbung
	, module FiniteMap
	, module Graph.Graph
	) 
	where

import Graph.Type
import Graph.Util
import Graph.Viz

import qualified Graph.Valid
import qualified Graph.Labeling

import Challenger

import ToDoc
import Data.Set
import Data.FiniteMap
import Sort
import Control.Monad ( guard ) -- old style

data DreiCol = DreiCol deriving Show

-- | Färbung als Abbildung : Knotemengen -> Zahlen
type Faerbung a = Graph.Labeling.Labeling a Integer



instance ( ToDoc (Graph a) , Show (Graph a) , Read (Graph a)
	, ToDoc (Faerbung a),Show (Faerbung a), Read (Faerbung a)
	, Ord a, ToDoc a, Show a, ShowText a
	, ToDoc [a]
	) => Problem DreiCol (Graph a) (Faerbung a) where

    getInstanz DreiCol graph loesung dateiName =
        getGraphviz graph instanzTrans dateiName
	
    getBeweis DreiCol graph loesung dateiName =
		-- Komplexität ist noch verbesserunswürdig
	getGraphviz graph (getBeweisTrans loesung) dateiName

    validiere DreiCol g f =
	 let ft1 @ (f1, t1) = Graph.Valid.valid g        
	     ft2 @ (f2, t2) = Graph.Labeling.valid g f       
	 in  ( f1 && f2 , t1 <+> t2 )

    verifiziere DreiCol g f =
        let 
	    range  = mkSet $ eltsFM f
	    erlaubt = mkSet [ 1 .. 3 ]
	    fehlfarben = minusSet range erlaubt
	    same = do k <- setToList $ kanten g
		      let x = von k; y = nach k
		      guard $  Graph.Labeling.the (lookupFM f x) 
			    == Graph.Labeling.the (lookupFM f y)
		      return k
	    ft @ ( flag, txt ) = Graph.Labeling.valid g f	

	in  if not flag then ft
	    else if not $ isEmptySet fehlfarben
	    then ( False, fsep [ text "Erlaubt sind nur die Farben:" 
				      <+> toDoc erlaubt
			       , text "aber nicht:" <+> toDoc fehlfarben
			       ] )
	    else if not $ null same 
	    then ( False, text "Diese Kanten haben gleichfarbige Endpunkte:"
			  <+> toDoc same )
	    else ( True , text "Das ist eine korrekte Drei-Färbung." )


		
-------------------------------------------------------------------------------
-- hier folgen Transformationen
-------------------------------------------------------------------------------

-- ganz einfache Transformation
instanzTrans :: ShowText knoten => GVTrans knoten
instanzTrans = GVTrans
	{ getGVProg = Neato
	, getGVFormat = "png"
	, isGVDirected = False
	, getGVNID = showText
	, getGVNName = showText
	, getGVNLabel = Nothing
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- um Label erweiterte Transformation
getBeweisTrans :: (ShowText knoten, Ord knoten)
	=> Faerbung knoten -> GVTrans knoten
getBeweisTrans loesung = GVTrans
	{ getGVProg = Neato
	, getGVFormat = "png"
	, isGVDirected = False
	, getGVNID = showText
	, getGVNName = showText
	, getGVNLabel = Nothing
	, getGVNColor = Just (getNColor loesung)
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- sucht zu einem Knoten die Farbe entsprechend der Klasse
getNColor :: Ord knoten => Faerbung knoten -> knoten -> GVColor
getNColor f knoten = getColor (Graph.Labeling.the $ lookupFM f knoten)

-- gibt eine Farbe zu einem Index einer Klasse zurück
-- sind erst mal nur 8 Farben - sollte aber reichen
getColor :: Integer -> GVColor
getColor index
	| index == 1 = "red"
	| index == 2 = "blue"
	| index == 3 = "green"

	| index == 4 = "yellow"
	| index == 5 = "magenta"
	| index == 6 = "navy"
	| index == 7 = "seagreen"
	| index == 8 = "purple"
	| otherwise = "white"

