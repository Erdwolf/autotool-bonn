-------------------------------------------------------------------------------
--
-- Modul, welches eine Instanz eines Graceful Problems behandelt
--
-- Einfach ausgedrückt schaut das Modul, ob ein gegebener Graph mit dem
-- gegebenen Labeling graceful (schön, hübsch) gelabelt ist.
--
--
-- Autor: Alexander Kiel
-- Version: 26.05.2002 - 3
-------------------------------------------------------------------------------

module Graceful.Graceful
	( Graceful
	, module Graph.Graph
	, module Graceful.Labeling
	, validiere
	, verifiziere
	, getInstanz
	, getBeweis
	, getDiffList
	) 
    where

import Graceful.Labeling
import Graph.Type
import Graph.Util
import Graph.Viz
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

instance
	( Ord knoten, Show knoten, ShowText knoten, ToDoc knoten
	, ToDoc (Graph knoten), Show (Graph knoten), Read (Graph knoten)
	, ToDoc (Labeling knoten), Show (Labeling knoten), Read (Labeling knoten)
	)
	=> Problem Graceful (Graph knoten) (Labeling knoten) where


	validiere Graceful graph labeling =
		testeAlles (text "Der Graph ist valid.")
			[ kantenPassenZuKnotenTest $ kantenPassenZuKnoten graph
			, ( isZusammen graph
			  , text "Der Graph ist nicht zusammenhängend."
			  )
			]
    
	verifiziere Graceful graph labeling =
		testeAlles (text "Die Lösung ist richtig.") (
			[ isLabelingAbbildungVonKnoten graph labeling
			, isSmallestLabelNull labeling
			, isBiggestLabelAnzKanten graph labeling
			, isLabelingEineindeutig labeling
			, isDiffListTight $ sort $ getDiffList graph labeling
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
	, isGVDirected = True
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
	, isGVDirected = True
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
getNLabel labeling knoten = show (getLabel labeling knoten)

-- Kantenlabel ist Graceful spezifisch
getELabel :: Ord a => (Labeling a) -> (Kante a) -> GVLabel
getELabel labeling kante = show (getDiff labeling kante)

-------------------------------------------------------------------------------
-- hier folgen die einzelnen Tests
-------------------------------------------------------------------------------

kantenPassenZuKnotenTest :: ToDoc knoten => (Bool, Maybe knoten) -> (Bool, Doc)
kantenPassenZuKnotenTest (bool, Nothing) = (bool, text "")
kantenPassenZuKnotenTest (bool, Just knoten) =
	( bool
	, text "Es gibt Kanten, die zu dem Knoten" <+>
      ( toDoc $ fromJust (Just knoten) ) <+>
	  text "zeigen, den es im Graph nicht gibt."
    ) 

isLabelingAbbildungVonKnoten :: Ord knoten 
	=> Graph knoten -> Labeling knoten -> (Bool, Doc)
isLabelingAbbildungVonKnoten graph labeling =
	( knoten graph == knotenSet labeling
	, text "Das Labeling ist keine Abbildung von der Menge der Knoten"
	)
	
isLabelingEineindeutig :: Ord knoten => Labeling knoten -> (Bool, Doc)
isLabelingEineindeutig labeling =
	( sizeL labeling == (cardinality $ labelSet labeling)
	, text "Das Labeling ist keine eineindeutige Abbildung"
	)


-- die Labels müssen mit der Zahl 0 anfangen
isSmallestLabelNull :: Labeling knoten -> (Bool, Doc)
isSmallestLabelNull labeling =
    ( (==) 0 $ head $ getSortedLabelList labeling
    , text "Das kleinste Label muss 0 sein."
    )

isBiggestLabelAnzKanten :: Graph knoten -> Labeling knoten -> (Bool, Doc)
isBiggestLabelAnzKanten graph labeling =
	( (==)
		(cardinality $ kanten graph)
		(fromInteger $ last $ getSortedLabelList labeling)
	, text
		( "Das gößte Label muss mit der Anzahl der Kanten ("
		++show (cardinality (kanten graph)) ++ ") übereinstimmen."
		)
	)

isDiffListTight :: [Integer] -> (Bool, Doc)
isDiffListTight diffList =
    ( isListTight diffList 1
    , text
		( "Die Menge der Differenzen ist nicht dicht gegenüber den "
		++"natürlichen Zahlen."
		)
    )

-------------------------------------------------------------------------------
-- hier folgen Hilfsfunktionen
-------------------------------------------------------------------------------

-- gibt eine Liste mit den Differenzen, welche an den Kanten stehen zurück
-- die Elemente der Liste sind nicht geordnet
getDiffList :: (Ord knoten) => (Graph knoten) -> (Labeling knoten) -> [Integer]
getDiffList graph labeling =
	[getDiff labeling kante | kante <- setToList(kanten graph)]

-- führt alle Tests durch und gibt entweder:
--   die Ausgabe (Bool, Doc) des ersten nicht bestanden Tests zurück
--   oder die positiv Meldung in der Form (True, Doc) zurück
testeAlles :: Doc -> [(Bool, Doc)] -> (Bool, Doc)
testeAlles positivDoc tests =
	getFirstError positivDoc [(b, doc) | (b, doc) <- tests, b == False]
    
getFirstError :: Doc -> [(Bool, Doc)] -> (Bool, Doc)
getFirstError positivDoc [] = (True, positivDoc)
getFirstError positivDoc es = head es

-- prüft, ob eine Liste gegenüber den natürlichen Zahlen ab einem bestimmten
-- Anfangswert dich ist
-- ist die Liste muss sortiert sein!
isListTight :: [Integer] -> Integer -> Bool
isListTight (x:xs) y = if x == y then isListTight xs (y+1) else False
isListTight [] _ = True
