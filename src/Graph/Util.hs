--  $Id$

module Graph.Util 

( module Graph.Util
, module Autolib.Graph.Type
, module Autolib.Reporter
, module Autolib.Reporter.Subset
, module Autolib.ToDoc
, module Autolib.FiniteMap
)

where

import Autolib.Graph.Type hiding ( iso )
import Autolib.Reporter 
import Autolib.Reporter.Subset

import Autolib.FiniteMap
import Autolib.ToDoc

validate :: GraphC a
	 => Graph a
	 -> Reporter ()
validate g = do
    let outs = do
          k <- lkanten g
	  guard $ not (  von  k `elementOf` knoten g
	              && nach k `elementOf` knoten g
		      )
          return k
    when ( not $ null outs ) $ reject $ vcat
         [ text "diese Kanten benutzen nicht deklarierte Knoten:"
	 , nest 4 $ toDoc outs
	 ]
    let loops = do
          k <- lkanten g
	  guard $ von k == nach k
	  return k
    when ( not $ null loops ) $ reject $ vcat
         [ text "diese Kanten sind Schlingen:"
         , nest 4 $ toDoc loops
	 ]


equal_set :: ( Ord a, ToDoc [a] )
       => ( Doc, Set a )
       -> ( Doc, Set a )
       -> Reporter ()
equal_set x @ (d1, s1) y @ (d2, s2) = do
    inform $ vcat
           [ text "Stimmen die Menge"
           , nest 4 $ fsep [ d1 , equals, toDoc s1 ]
           , text "und die Menge"
           , nest 4 $ fsep [ d2, equals, toDoc s2 ]
           , text "�berein?"
           ]
    Autolib.Reporter.Subset.check x y
    Autolib.Reporter.Subset.check y x

nachbarn :: Ord a => Graph a -> a -> Set a
nachbarn g x = mkSet $ do
    k <- lkanten g
    if x == von k then return $ nach k
       else if x == nach k then return $ von k
	  else mzero

degree :: Ord a => Graph a -> a -> Int
degree g x = cardinality $ nachbarn g x

lkanten g = setToList $ kanten g
lknoten g = setToList $ knoten g

check_reg :: ( ToDoc a, ToDoc [a], Ord a )
	  => Graph a
	  -> Reporter ()
check_reg g = do
    inform $ fsep [ text "ist der Graph", info g, text "regul�r?" ]
    let degs = fmToList $ addListToFM_C union emptyFM $ do
           x <- lknoten g
	   return (degree g x, unitSet x)
    if length degs > 1
       then reject $ text "nein, die Knotengrade sind" $$ toDoc degs
       else inform $ text "ja."


