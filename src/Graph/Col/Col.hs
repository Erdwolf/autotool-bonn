-- autor m.lindemeyer
-- stinfwww.infomatik.uni-leipzig.de/~psy99hvr
-- (8484955)

module Col.Col 
	( Col, Klassen 
	, module Graph.Graph
	, validiere
	, verifiziere
	, getInstanz
	, getBeweis
	) 
	where

import Graph.Type
import Graph.Util
import Graph.Viz
import Challenger
import ToDoc
import Set
import Sort
import Monad ( guard ) -- old style

data Col = Col deriving Show
data Klassen a = Klassen [[a]] deriving (Read,Show)

instance (ToDoc [a])=> ToDoc (Klassen a) where
	toDoc (Klassen erg) = text "Loesung: " <+> toDoc erg


instance
	(ToDoc ((Graph a, Integer)), Show ((Graph a, Integer)), Read ((Graph a, Integer)), Iso ((Graph a, Integer))
	, ToDoc (Klassen a),Show (Klassen a), Read (Klassen a), Ord a, ToDoc a
	, Show a, ShowText a)
	=> Problem Col (Graph a, Integer) (Klassen a) where

	validiere Col (g,anzahl) lsg =
		let
			(kpassen, fehlknoten) = kantenPassenZuKnoten g
			zusammen = isZusammen g
		in
			if and [kpassen, zusammen]
				then (True, text "Graph Ok.")
				else
					( False
					, maybe
						(text "Dein Graph ist kein zusammenhaengender Graph.")
						(\x -> text
							("In deinem Graph existiert mindestents eine Kante "
							++ "mit falschem Knoten: "
							++ show x
							)
						)
						fehlknoten
					)

	verifiziere Col (g,anzahl) (Klassen  lsg) = colTest g anzahl lsg

	getInstanz Col (graph,anzahl) (Klassen  loesung) dateiName =
		getGraphviz graph instanzTrans dateiName
	
	getBeweis Col (graph,anzahl) loesung dateiName =
		-- Komplexität ist noch verbesserunswürdig
		getGraphviz graph (getBeweisTrans (sortLoesung $ loesung)) dateiName
		
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
	=> Klassen knoten -> GVTrans knoten
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
getNColor :: Eq knoten => Klassen knoten -> knoten -> GVColor
getNColor (Klassen liste) knoten =
	getColor $ findKlasse liste knoten 1 -- erste Klasse hat Index 1
	where
		findKlasse :: Eq a => [[a]] -> a -> Int -> Int
		findKlasse (x:xs) knoten index
			| elem knoten x = index
			| otherwise = findKlasse xs knoten (index + 1)
		findKlasse [] knoten index = (-1) -- nicht gefunden

-- gibt eine Farbe zu einem Index einer Klasse zurück
-- sind erst mal nur 8 Farben - sollte aber reichen
getColor :: Int -> GVColor
getColor index
	| index == 1 = "red"
	| index == 2 = "blue"
	| index == 3 = "yellow"
	| index == 4 = "green"
	| index == 5 = "magenta"
	| index == 6 = "navy"
	| index == 7 = "seagreen"
	| index == 8 = "purple"
	| otherwise = "white"

-- sortiert sowohl die Knoten in den Klassen als auch die Klassen selbst
sortLoesung :: Ord knoten => Klassen knoten -> Klassen knoten
sortLoesung (Klassen liste) =
	(Klassen (sort $ sortInner liste))
	where
		sortInner :: Ord a => [[a]] -> [[a]]
		sortInner (x:xs) = (sort x) : (sortInner xs)
		sortInner [] = []

------------------------------------------------------------------------------------------------------------
--Probe, ob Knoten der Loesung mit Knoten des Graphen uebereinstimmen
colTest :: (ToDoc a, Ord a) => Graph a -> Integer -> [[a]] -> (Bool,Doc)
colTest g anzahl b = 
  if (anzahl /= 3) || (length b /= 3) 
    then (False, text "Die Faerbung ist unkorrekt! Es werden nur 3 Farbungen akzepztiert!")
    else 
      if not((sort(knotenliste g))==(sort(concat b)))
        then (False, text "Nicht alle oder zuviele Knoten sind in der Loesung enthalten.")
        else if ([]==(klassenTest g b))
            then (True, text "Herzlichen Glueckwunsch, die Faerbung ist korrekt.")
            else (False, text "Die Faerbung ist unkorrekt! Mindestens ein Knotenpaar ist in einer Klasse.")


------------------------------------------------------------------------------------------------------------
--eigentliche Colorisierungs-Testfunktion
klassenTest :: (ToDoc a, Ord a, Eq a) => Graph a -> [[a]] -> [Int]
klassenTest g li = do
                  x <- (knotenliste g)
                  y <- (nachfolger g x)
                  guard $ not $ elem y (concat(listeohnex li x))
                  return 0

------------------------------------------------------------------------------------------------------------
--aus einer liste einer liste wird eine liste geloescht
listeohnex :: Eq a => [[a]] -> a -> [[a]]
listeohnex a b = do
                  z <- a
                  if (func1 z b == z)
                    then return z
                    else return []
                  
func1 :: Eq a => [a] -> a -> [a]
func1 z b = do
            y <- z
            guard $ not $ y==b
            return y

--beispielgraphen
bsp_Cgraph = Graph {
    knoten = mkSet [1, 2, 3, 4, 5],
    kanten = mkSet [kante 1 2, kante 2 3, kante 3 4, kante 4 5, kante 3 5, kante 1 5]
}
bsp_Cfaerb = Klassen [[1,4],[2,5],[3]]


--falsch
bsp_Cgraph2 = Graph {
    knoten = mkSet [1, 2, 3, 4, 5],
    kanten = mkSet [kante 1 2, kante 2 3, kante 3 4, kante 4 5, kante 3 5, kante 1 5]
}
bsp_Cfaerb2 = Klassen [[2,4],[1,5],[3]]

