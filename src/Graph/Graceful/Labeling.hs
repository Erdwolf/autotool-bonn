--------------------------------------------------------------------------------
--
-- Modul, welches die Datenstruktur Labeling mit Funktionen bereitstellt
--
-- Ein Labeling ist eine eindeutige Abbildung von Knoten eines Graphen in die
-- Menge der nat�rlichen Zahlen.
--
-- Implementiert ist das Labeling zur Zeit mit einer FiniteMap, was von Au�en
-- allerdings nicht interessant ist. Ein Konten kann einen beliebigen Typ haben,
-- wenn dieser einige Eigenschaften wie Ord hat. Das Label hingegen ist auf den
-- festen Typ Integer gesetzt.
--
-- Autor: Alexander Kiel
-- Version: 25.05.2002

--------------------------------------------------------------------------------

-- ge�ndert: ToDoc Labeling joe@informatik.uni-leipzig.de


module Graceful.Labeling
	( Labeling
	, emptyLabeling
	, unitLabeling
	, mkLabeling
	, sizeL
	, lToList
	, knotenSet
	, labelSet
	, getSortedLabelList
	, getLabel
	, getDiff
	)
	where

import Graph.Graph
import Data.FiniteMap
import ToDoc
import Maybe
import ReadFM
import Sort

newtype Labeling knoten = Labeling
	( FiniteMap knoten Integer
	) -- deriving ( Show, Read) -- siehe unten


emptyLabeling :: Labeling knoten
emptyLabeling = Labeling emptyFM

unitLabeling :: (knoten, Integer) -> Labeling knoten
unitLabeling (knoten, int) = Labeling (unitFM knoten int)

-- konstruiert ein Labeling aus einer Liste von Tupeln (knoten, Integer)
-- die letzte Abbildung von einem Knoten auf ein Label gewinnt
mkLabeling :: Ord knoten => [(knoten, Integer)] -> Labeling knoten
mkLabeling liste = Labeling (listToFM liste)

-- gibt die Anzahl der Abbildungen im Labeling zur�ck
sizeL :: Labeling knoten -> Int
sizeL (Labeling fmap) = sizeFM fmap 

lToList :: Labeling knoten -> [(knoten, Integer)]
lToList (Labeling fmap) = fmToList fmap

knotenSet :: Ord knoten => Labeling knoten -> Set knoten
knotenSet labeling = mkSet [knoten | (knoten, label) <- lToList labeling]

labelSet :: Ord knoten => Labeling knoten -> Set Integer
labelSet labeling = mkSet [label | (knoten, label) <- lToList labeling]

-- gibt die Label der Knoten in einer geordneten Liste mit dem kleinten Label
-- zuerst zur�ck
getSortedLabelList :: Labeling knoten -> [Integer]
getSortedLabelList (Labeling fmap) = sort $ eltsFM fmap

-- gibt das Label eines Knotens zur�ck
-- gibt -1 zur�ck, falls Label nicht gefunden wurde
getLabel :: Ord knoten => Labeling knoten -> knoten -> Integer
getLabel (Labeling fmap) knoten = 
	maybe (-1) id (lookupFM fmap knoten)

-- gibt die Differenz einer Kante zur�ck
-- gibt -1 zur�ck, falls eines der beiden Label nicht gefunden wurde
getDiff :: Ord knoten => Labeling knoten -> Kante knoten -> Integer
getDiff labeling kante =
	labelDiff (getLabel labeling (von kante)) (getLabel labeling (nach kante))

-- spezielle Differenz
labelDiff :: Integer -> Integer -> Integer
labelDiff (-1) _ = (-1)
labelDiff _ (-1) = (-1)
labelDiff label1 label2 = abs (label1 - label2)

--------------------------------------------------------------------------------
-- ToDoc Implementation f�r Labeling
--------------------------------------------------------------------------------

instance ToDoc knoten => ToDoc ( Labeling knoten ) where
    toDoc ( Labeling fm ) = text "mkLabeling" <+> toDoc ( fmToList fm )
instance  ToDoc knoten => Show (  Labeling knoten ) where
    show = render . toDoc

instance (Ord knoten, Read knoten) => Read ( Labeling knoten ) where
    readsPrec p cs = do
        ( l, cs ) <- lex cs
	if l == "Labeling" then do
	     -- da� das jemals funkionierte ... now deprecated!
	     ( fm, cs ) <- reads cs
	     return ( Labeling fm, cs )
	 else if l == "mkLabeling" then do
	      ( pairs, cs ) <- reads cs
	      return ( mkLabeling pairs, cs )
	 else []
