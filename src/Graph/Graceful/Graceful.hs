-------------------------------------------------------------------------------
--
-- Modul, welches eine Instanz eines Graceful Problems behandelt
--
-- Einfach ausgedrückt schaut das Modul, ob ein gegebener Graph mit dem
-- gegebenen Labeling graceful (schön, hübsch) gelabelt ist.
--
--
-- Autor: Alexander Kiel
-- Version: 28.05.2002 - 1
-------------------------------------------------------------------------------

module Graceful.Graceful
	( Graceful
	, module Graph.Graph
--	, module Graceful.Labeling
	, module FiniteMap
	) 
    where

-- import Graceful.Labeling
import Graph.Type
import Graph.Util
import Graph.Viz

import qualified Graph.Valid
import qualified Graph.Labeling

import Challenger
import ToDoc
import Set
import Sort
import Maybe
import FiniteMap

data Graceful = Graceful deriving Show


-- Instanzierung der Klasse Problem (problem, instanz, beweis)
--   eine Instanz muss hier ein (Graph a) sein, also ein Graph mit beliebigem
--     Knotentyp
--   ein Beweis muss hier ein (Labeling a) sein, also eine Abbildung von dem
--     beliebigem Knotentyp a auf  

type Labeling a = Graph.Labeling.Labeling a Integer

instance
    ( Ord knoten, Show knoten, ShowText knoten, ToDoc knoten
    , ToDoc (Graph knoten), Show (Graph knoten), Read (Graph knoten)
    , Read (FiniteMap knoten Integer)
    , ToDoc [knoten]
--    , Number (Graph knoten) (Graph Int), Show (Graph Int), Read (Graph Int)
--    , ToDoc (Labeling knoten), Show (Labeling knoten), Read (Labeling knoten)
    )
    => Problem Graceful (Graph knoten) (Labeling knoten) where


    validiere Graceful graph labeling = 
	 let (f1, t1) = Graph.Valid.valid graph
	     (f2, t2) = Graph.Labeling.valid graph labeling
	     (f3, t3) = Graph.Labeling.injektiv labeling
	 in  ( and [f1, f2, f3] 
	     , fsep [ t1,  t2, t3 ] 
	     )

{-    
	    testeAlles (text "Der Graph ist valid.")
		    [ kantenPassenZuKnotenTest $ kantenPassenZuKnoten graph
		    , ( isZusammen graph
		      , text "Der Graph ist nicht zusammenhängend."
		      )
		    ]
-}

    verifiziere Graceful graph labeling =
	    testeAlles (text "Die Lösung ist richtig.") (
		    [
		    -- isLabelingAbbildungVonKnoten graph labeling
		      isSmallestLabelNull labeling
		    , isBiggestLabelAnzKanten graph labeling
		    -- , isLabelingEineindeutig labeling
		    -- , isDiffListTight $ sort $ getDiffList graph labeling
		    , dicht graph labeling
		    ])

    getInstanz Graceful graph labeling dateiName =
	    -- mehr muss hier keiner machen, der Graphviz nutzt
	    getGraphviz graph instanzTrans dateiName

    getBeweis Graceful graph labeling dateiName =
	    getGraphviz graph (getBeweisTrans labeling) dateiName

		
-------------------------------------------------------------------------------
-- hier folgen Transformationen
-------------------------------------------------------------------------------

-- ganz einfache Transformation
instanzTrans :: ShowText knoten => GVTrans knoten
instanzTrans = GVTrans
	{ getGVProg = Neato
	, getGVFormat = "png"
	, isGVDirected = False
	, getGVNID = showText		-- Graphviz Knoten ID ist einfach show knoten
	, getGVNName = showText		-- Knotenname auch
	, getGVNLabel = Nothing
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- um Label erweiterte Transformation
getBeweisTrans :: (ShowText knoten, Ord knoten)
	=> (Labeling knoten) -> (GVTrans knoten)
getBeweisTrans labeling = GVTrans
	{ getGVProg = Neato
	, getGVFormat = "png"
	, isGVDirected = False
	, getGVNID = showText
	, getGVNName = showText
	, getGVNLabel = Just (getNLabel labeling)
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Just (getELabel labeling)
	, getGVEXAtts = Nothing
	}

-- Knotenlabel ist Graceful spezifisch
getNLabel :: Ord a => (Labeling a) -> a -> GVLabel
getNLabel labeling knoten = show (Graph.Labeling.the $ lookupFM labeling knoten)

-- Kantenlabel ist Graceful spezifisch
getELabel :: Ord a => (Labeling a) -> (Kante a) -> GVLabel
getELabel labeling kante = show (getDiff labeling kante)

-------------------------------------------------------------------------------
-- hier folgen die einzelnen Tests
-------------------------------------------------------------------------------

-- die Labels müssen mit der Zahl 0 anfangen
isSmallestLabelNull :: Labeling knoten -> (Bool, Doc)
isSmallestLabelNull labeling =
    ( (==) 0 $ minimum $ eltsFM labeling
    , text "Das kleinste Label muss 0 sein."
    )

isBiggestLabelAnzKanten :: Graph knoten -> Labeling knoten -> (Bool, Doc)
isBiggestLabelAnzKanten graph labeling =
	( (==)
		(cardinality $ kanten graph)
		(fromInteger $ maximum $ eltsFM labeling)
	, text
		( "Das größte Label muss mit der Anzahl der Kanten ("
		++show (cardinality (kanten graph)) ++ ") übereinstimmen."
		)
	)

getDiff :: Ord a => Labeling a -> Kante a -> Integer
getDiff f k =
    let x = Graph.Labeling.the $ lookupFM f $ von  k
	y = Graph.Labeling.the $ lookupFM f $ nach k
    in	abs (x - y)

dicht :: (Ord a, ToDoc a) => Graph a -> Labeling a -> ( Bool, Doc )
dicht g f = 
    let diffs = listToFM $ do 
		   k <- setToList $ kanten g
		   return ( k, getDiff f k )
	( fl, t ) = Graph.Labeling.injektiv diffs
    in	( fl, text "Für die Abbildung  Kante -> Differenz gilt:" <+> t )
    
--------------------------------------------------------------------------


-- führt alle Tests durch und gibt entweder:
--   die Ausgabe (Bool, Doc) des ersten nicht bestanden Tests zurück
--   oder die positiv Meldung in der Form (True, Doc) zurück
testeAlles :: Doc -> [(Bool, Doc)] -> (Bool, Doc)
testeAlles positivDoc tests =
	getFirstError positivDoc [(b, doc) | (b, doc) <- tests, b == False]
    
getFirstError :: Doc -> [(Bool, Doc)] -> (Bool, Doc)
getFirstError positivDoc [] = (True, positivDoc)
getFirstError positivDoc es = head es

