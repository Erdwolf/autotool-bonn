-- autor m.lindemeyer
-- stinfwww.informatik.uni-leipzig.de/~psy99hvr
-- (8484955)

-- patches: joe@informatik.uni-leipzig.de

module Col.DreiCol
	( DreiCol, Faerbung
	, module Graph.Graph
	) 
	where

import Graph.Type
import Graph.Util
import Graph.Viz
import Graph.Valid
import Challenger

import ToDoc
import Set
import FiniteMap
import Sort
import Monad ( guard ) -- old style

data DreiCol = DreiCol deriving Show

-- Färbung als Abbildung : Knotemengen -> Zahlen
type Faerbung a = FiniteMap a Integer


symdiff :: Ord a => Set a -> Set a -> (Set a , Set a)
symdiff xs ys = ( minusSet xs ys, minusSet ys xs )

the :: Maybe a -> a
the (Just x) = x
the Nothing = error "Faerbung.the"

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
         valid g        

    verifiziere DreiCol g f =
        let domain = mkSet $ keysFM f
	    range  = mkSet $ eltsFM f
	    erlaubt = mkSet [ 1 .. 3 ]
	    fehlfarben = minusSet range erlaubt

	    (here, there) = symdiff (knoten g) domain
	    same = do k <- setToList $ kanten g
		      let x = von k; y = nach k
		      guard $ the (lookupFM f x) == the (lookupFM f y)
		      return k
	
	in  if not $ isEmptySet here
	    then ( False, text "Diese Knoten sind nicht gefärbt:"
				  <+> toDoc here )
	    else if not $ isEmptySet there
	    then ( False, text "Das sind gar keine Knoten des Graphen:"
				  <+> toDoc there )
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
getNColor f knoten = getColor (the $ lookupFM f knoten)

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

