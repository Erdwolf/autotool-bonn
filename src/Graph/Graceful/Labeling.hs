--------------------------------------------------------------------------------
--
-- Modul, welches die Datenstruktur Labeling mit Funktionen bereitstellt
--
-- Ein Labeling ist eine eindeutige Abbildung von Knoten eines Graphen in die
-- Menge der natürlichen Zahlen.
--
-- Implementiert ist das Labeling zur Zeit mit einer FiniteMap, was von Außen
-- allerdings nicht interessant ist. Ein Konten kann einen beliebigen Typ haben,
-- wenn dieser einige Eigenschaften wie Ord hat. Das Label hingegen ist auf den
-- festen Typ Integer gesetzt.
--
-- Autor: Alexander Kiel
-- Version: 22.05.2002
--------------------------------------------------------------------------------


module Graceful.Labeling
	( Labeling
	, mkLabeling
	, sizeL
	, getSortedLabelList
	, getLabel
	, getDiff
	)
	where

import Graph.Graph
import FiniteMap
import ToDoc
import Maybe
import ReadFM
import Sort

data Labeling knoten = Labeling
	( FiniteMap knoten Integer
	) deriving (Show, Read)


-- konstruiert ein Labeling aus einer Liste von Tupeln (knoten, Integer)
-- die letzte Abbildung von einem Knoten auf ein Label gewinnt
mkLabeling :: Ord knoten => [(knoten, Integer)] -> Labeling knoten
mkLabeling liste = Labeling (listToFM liste)

-- gibt die Anzahl der Abbildungen im Labeling zurück
sizeL :: Labeling knoten -> Int
sizeL (Labeling fmap) = sizeFM fmap 

-- gibt die Label der Knoten in einer geordneten Liste mit dem kleinten Label
-- zuerst zurück
getSortedLabelList :: Labeling knoten -> [Integer]
getSortedLabelList (Labeling fmap) =
	sort $ eltsFM fmap

-- gibt das Label eines Knotens zurück
-- gibt -1 zurück, falls Label nicht gefunden wurde
getLabel :: Ord knoten => Labeling knoten -> knoten -> Integer
getLabel (Labeling fmap) knoten = 
	maybe (-1) id (lookupFM fmap knoten)

-- gibt die Differenz einer Kante zurück
-- gibt -1 zurück, falls eines der beiden Label nicht gefunden wurde
getDiff :: Ord knoten => Labeling knoten -> Kante knoten -> Integer
getDiff labeling kante =
	labelDiff (getLabel labeling (von kante)) (getLabel labeling (nach kante))

-- spezielle Differenz
labelDiff :: Integer -> Integer -> Integer
labelDiff (-1) _ = (-1)
labelDiff _ (-1) = (-1)
labelDiff label1 label2 = abs (label1 - label2)

--------------------------------------------------------------------------------
-- ToDoc Implementation für Labeling
--------------------------------------------------------------------------------

instance (ToDoc knoten, ToDoc (FiniteMap knoten Integer))
	=> ToDoc (Labeling knoten) where
	toDoc (Labeling fmap) =
		text "Labeling" <+> (toDoc fmap)
