-------------------------------------------------------------------------------
--
-- Modul, welches eine Instanz eines Graceful Problems behandelt
--
-- Einfach ausgedrückt schaut das Modul, ob ein gegebener Graph mit dem
-- gegebenen Labeling graceful (schön, hübsch) gelabelt ist.
--
--
-- Autor: Alexander Kiel
-- Version: 11.05.2002
-------------------------------------------------------------------------------

module Graceful.Graceful
	( Graceful
        , module Graceful.Labeling
	, module Graph.Type
	, validiere
	, verifiziere
	, getInstanz
	, getBeweis
	) 
    where

import Graceful.Labeling
import Graph.Type
import Graph.Util
import Graph.Viz
import Challenger
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
	( Ord knoten, Show knoten, ToDoc knoten
	, ToDoc (Graph knoten), Show (Graph knoten), Read (Graph knoten)
	, ToDoc (Labeling knoten), Show (Labeling knoten), Read (Labeling knoten)
	)
	=> Problem Graceful (Graph knoten) (Labeling knoten) where


	validiere Graceful graph labeling =
		testeAlles (text "Der Graph ist valid.") (
			[ kantenPassenZuKnotenTest $ kantenPassenZuKnoten graph
			, ( isZusammen graph
			  , text "Der Graph ist nicht zusammenhängend."
			  )
			])
    
	verifiziere Graceful graph labeling =
		testeAlles (text "Die Lösung ist richtig.") (
			[ isAnzKnotenEqLengthLabeling graph labeling
			, isSmallestLabelNull labeling
			, isDiffListTight $ sort $ getDiffList graph labeling
			])

	getInstanz Graceful graph labeling dateiName =
		-- mehr muss hier keiner machen, der Graphviz nutzt
		getGraphviz graph getInstanzTrans dateiName "gif"
	
	getBeweis Graceful graph labeling dateiName =
		getGraphviz graph (getBeweisTrans labeling) dateiName "gif"	
		
-------------------------------------------------------------------------------
-- hier folgen Transformationen
-------------------------------------------------------------------------------

-- ganz einfache Transformation
getInstanzTrans :: Show knoten => GVTrans knoten
getInstanzTrans = GVTrans
	{ getGVNID = show		-- Graphviz Knoten ID ist einfach show knoten
	, getGVNName = show		-- Knotenname auch
	, getGVNLabel = Nothing
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, isGVEDirected = (\x -> False)	--alle Kanten ungerichtet
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- um Label erweiterte Transformation
getBeweisTrans :: (Show knoten, Ord knoten)
	=> (Labeling knoten) -> (GVTrans knoten)
getBeweisTrans labeling = GVTrans
	{ getGVNID = show
	, getGVNName = show
	, getGVNLabel = Just (getNLabel labeling)
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, isGVEDirected = (\x -> False)
	, getGVELabel = Just (getELabel labeling)
	, getGVEXAtts = Nothing
	}

-- Knotenlabel ist Graceful spezifisch
getNLabel :: Ord a => (Labeling a) -> a -> GVLabel
getNLabel labeling knoten = '(' : shows (getLabel labeling knoten) ")"

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

isAnzKnotenEqLengthLabeling :: (Graph a) -> (Labeling a) -> (Bool, Doc)
isAnzKnotenEqLengthLabeling graph labeling =
    ( (anzKnoten graph == sizeL labeling)
    , text "Anzahl der Knoten stimmt nicht mit der Anzahl der Labels überein."
    )

-- die Labels müssen mit der Zahl 0 anfangen
isSmallestLabelNull :: (Labeling a) -> (Bool, Doc)
isSmallestLabelNull labeling =
    ( (==) 0 $ head $ getSortedLabelList labeling
    , text "Das erste Label muss 0 sein."
    )

isDiffListTight :: [Integer] -> (Bool, Doc)
isDiffListTight diffList =
    ( isListTight diffList 1
    , text "Die Menge der Labels ist nicht dicht gegenüber den natürlichen Zahlen."
    )

isLabelingGraceful :: (Graph a) -> (Labeling a) -> (Bool, Doc)
isLabelingGraceful graph labeling =
    ( False
    , text ""
    )

-------------------------------------------------------------------------------
-- hier folgen Hilfsfunktionen
-------------------------------------------------------------------------------

-- gibt eine Liste mit den Differenzen, welche an den Kanten stehen zurück
-- die Elemente der Liste sind nicht geordnet
getDiffList :: (Ord knoten) => (Graph knoten) -> (Labeling knoten) -> [Integer]
getDiffList graph labeling =
	[ abs((getLabel labeling (von kante)) - (getLabel labeling (nach kante)))
	| kante <- setToList(kanten graph)
	]

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
isListTight [] _ = True
isListTight (x:xs) y = if x == y then isListTight xs (y+1) else False

-------------------------------------------------------------------------------
-- Graphviz Implementierungen
-------------------------------------------------------------------------------

-- erzeugt die gesammte Ausgabe für Graphviz
showGracefulGraph :: (Show knoten, Ord knoten)
	=> Graph knoten -> Labeling knoten -> String
showGracefulGraph graph labeling =
	( "graph G {\n"
	++"\tedge [len=1];\n"
    ++showsGracefulKanten (setToList (kanten graph)) labeling ""
    ++"}\n"
    )

-- die folgenden Funktionen arbeiten wie shows
-- damit die Komplexität linear beleibt
-- siehe Docu

-- erzeugt alle Kanten
showsGracefulKanten :: (Show knoten, Ord knoten)
	=> [Kante knoten] -> Labeling knoten -> ShowS
showsGracefulKanten (kante:[]) labeling = showsGracefulKante kante labeling
showsGracefulKanten (kante:kanten) labeling =
	showsGracefulKante kante labeling .
	showsGracefulKanten kanten labeling  

-- erzeugt eine Kante mit Differenz als Label
showsGracefulKante :: (Show knoten, Ord knoten)
	=> Kante knoten -> Labeling knoten -> ShowS
showsGracefulKante kante labeling =
	("\t" ++) .
	shows (showsKnoten labeling (von kante) "") .
	(" -- " ++) . 
	shows (showsKnoten labeling (nach kante) "") .
	(" [label=" ++) .
	shows (getDiff labeling kante) .
	("];\n" ++)
    
{-
showsKante :: Show knoten
	=> Kante knoten -> Maybe knotenLabel -> Maybe kantenLabel -> ShowS
showsKante kante Nothing Nothing =
	("\t" ++) . shows (showsKnoten (von kante) Nothing "") .
    shows (showsKnoten (nach kante) Nothing "") . (";\n" ++)
showsKante kante (Just knotenlabel) Nothing =
	("\t" ++) . shows (showsKnoten (von kante) (fromJust knotenlabel) "") .
    shows (showsKnoten (nach kante) Nothing "") . (";\n" ++)
-}

-- erzeugt einen Knoten in der Form "Knotenname (Label)"
showsKnoten :: (Show knoten, Ord knoten) => Labeling knoten -> knoten -> ShowS
showsKnoten labeling knoten =
	shows knoten . ('(':) . shows (getLabel labeling knoten) . (')':)

--showsKnoten :: (Show knoten, Show label) => knoten -> Maybe label -> ShowS
--showsKnoten knoten Nothing = shows knoten
--showsKnoten knoten label =
--	shows knoten . ('(':) . shows (fromJust label) . (')':)


