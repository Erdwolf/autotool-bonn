{-# language FlexibleContexts #-}

module Graph.Util 

( module Graph.Util
, module Autolib.Graph.Type
, module Autolib.Reporter
, module Autolib.Reporter.Set
, module Autolib.ToDoc
, module Autolib.FiniteMap
)

where

import Autolib.Graph.Type hiding ( iso )
import Autolib.Graph.Ops ( restrict , unlinks )
import Autolib.Graph.Util ( isZusammen , anzKnoten )
import Autolib.Graph.Adj ( AdjMatrix , adjazenz_matrix )
import Autolib.Reporter 
import Autolib.Reporter.Set

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Schichten
import Autolib.Set
import Autolib.Util.Fix ( fixL )

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
equal_set = Autolib.Reporter.Set.eq

nachbarn :: GraphC a => Graph a -> a -> Set a
nachbarn g x = mkSet $ lnachbarn g x

lnachbarn :: GraphC a => Graph a -> a -> [ a ]
lnachbarn g x =  do
    k <- lkanten g
    if x == von k then return $ nach k
       else if x == nach k then return $ von k
	  else mzero

reachables :: GraphC a => Graph a -> a -> Set a
reachables g x = unionManySets $ schichten ( nachbarn g ) x

-------------------------------------------------------------------------------
-- | echte nachfolger (nicht reflexive nachfolger-hülle)

sucs :: GraphC a => Graph a -> a -> Set a
sucs g = last . fixL . scanl union emptySet . tail 
	                                    . schichten_with_loops (nachbarn g)

-- | echte nachfolger, die genau n>0 schritte entfernt sind, n=0 -> keine!

sucsN :: GraphC a => Int -> Graph a -> a -> Set a
sucsN 0 _ _ = emptySet
sucsN n g x = schichten_with_loops (nachbarn g) x !! n

-- | gibt es nichtleeren pfad von x nach y?

path :: GraphC a => Graph a -> a -> a -> Bool
path g x y = y `elementOf` sucs g x

-- | gibt es pfad der länge n von x nach y

pathN :: GraphC a => Int -> Graph a -> a -> a -> Bool
pathN n g x y = y `elementOf` sucsN n g x

-- | erreichbarkeitsgraph: kanten zwischen allen knoten, zwischen
-- | denen ein nichtleerer pfad existiert
-- | es gilt: adjazenz_matrix . egraph == wegematrix

egraph :: GraphC a => Graph a -> Graph a
egraph g = gen_egraph (path g) g

wegematrix :: GraphC a => Graph a -> AdjMatrix
wegematrix = adjazenz_matrix . egraph

-- | erreichbarkeitsgraph: kanten zwischen allen knoten, zwischen
-- | denen ein Pfad der Länge n existiert
-- | es gilt: n>0 => adjazenz_matrix (egraphN n g) = sig (adjazenz_matrix g)^n

egraphN :: GraphC a => Int -> Graph a -> Graph a
egraphN n g = gen_egraph (pathN n g) g

gen_egraph :: GraphC a => ( a -> a -> Bool ) -> Graph a -> Graph a
gen_egraph f g = mkGraph (knoten g)
		         (mkSet $ do
			  (x,y) <- setToList $ cross (knoten g) (knoten g)
			  guard $ f x y
			  return $ kante x y
			 )

-------------------------------------------------------------------------------

degree :: GraphC a => Graph a -> a -> Int
degree g x = cardinality $ nachbarn g x

lkanten :: GraphC a => Graph a -> [Kante a]
lkanten = setToList . kanten

lknoten :: GraphC a => Graph a -> [a]
lknoten = setToList . knoten

is_clique :: ( ToDoc [a], GraphC a ) => Graph a -> Set a -> Bool
is_clique g s = 
    let h = restrict s g
	n = cardinality $ knoten h
	m = cardinality $ kanten h
    in  2 * m == n * pred n

is_independent :: GraphC a => Graph a -> Set a -> Bool
is_independent g s = 
    let h = restrict s g
	m = cardinality $ kanten h
    in  0 == m

is_connected :: GraphC a => Graph a -> Bool
is_connected g = 
    let xs = reachables g ( head $ lknoten g )
    in  xs == knoten g

check_reg :: ( ToDoc a, ToDoc [a], GraphC a )
	  => Graph a
	  -> Reporter ()
check_reg g = do
    inform $ fsep [ text "ist der Graph", info g, text "regulär?" ]
    let degs = fmToList $ addListToFM_C union emptyFM $ do
           x <- lknoten g
	   return (degree g x, unitSet x)
    if length degs > 1
       then reject $ text "nein, die Knotengrade sind" $$ toDoc degs
       else inform $ text "ja."

ist_clique :: GraphC a => Graph a -> Bool
ist_clique g = 
    let n = cardinality $ knoten g
        m = cardinality $ kanten g
    in  2 * m == n * pred n

no_fixed_layout :: GraphC a => Graph a -> Graph a
no_fixed_layout g = g { graph_layout = emptyFM
		      , layout_hints = []
		      }

-------------------------------------------------------------------------------

-- | radius und durchmesser in einem rutsch

rad_diam :: GraphC a => Graph a -> Maybe (Int,Int)
rad_diam g | not $ isZusammen g = Nothing
rad_diam g = let suc = nachbarn g
		 ls = map ( pred . length . schichten suc ) $ lknoten g
             in Just ( minimum ls , maximum ls )

-- | radius

rad :: GraphC a => Graph a -> Maybe Int
rad g = rad_diam g >>= return . fst

-- | durchmesser

diam :: GraphC a => Graph a -> Maybe Int
diam g = rad_diam g >>= return . snd

-------------------------------------------------------------------------------

-- | artikulationspunkte: knoten, die durch entfernen zu einem
-- | nicht-zusammenhängenden graphen führen. beachte: wenn der
-- | ausgangsgraph nicht zusammenhängend ist, dann sind alle knoten
-- | artikulationspunkte!

artikulationen :: GraphC a => Graph a -> [a]
artikulationen g = filter ( \ n -> not $ isZusammen 
		          $ restrict (delFromSet (knoten g) n) g 
			  )
		   $ setToList $ knoten g

-------------------------------------------------------------------------------

-- | einfach zusammenhängende komponenten als liste von graphen

komponenten :: GraphC a => Graph a -> [Graph a]
komponenten g
    | isEmptySet rs_quer = [g]
    | otherwise          = restrict rs g : komponenten (restrict rs_quer g)
    where v       = knoten g
	  x       = head $ setToList v
	  rs      = reachables g x
	  rs_quer = knoten g `minusSet` rs

-------------------------------------------------------------------------------

-- | bisektion: eine kantenmenge wird entfernt, sodass der
-- | resultierende graph in zwei teilgraphen (nicht komponenten!)
-- | zerfällt, die sich um höchstens eins in der knotenzahl
-- | unterscheiden und zwischen denen keine kanten verlaufen

bisektionen :: GraphC a => Graph a -> [ Set (Kante a) ]
bisektionen g = filter ( ist_bisektiert . unlinks g . setToList 
		       ) $ subsets $ kanten g

ist_bisektiert :: GraphC a => Graph a -> Bool
ist_bisektiert g = let n  = anzKnoten g
		       ns = teilmengen (div n 2) (knoten g)
		   in any (bisektierend g) ns

bisektierend :: GraphC a => Graph a -> Set a -> Bool
bisektierend g ns = all ( \ k -> not $ or
			  [ and [       von  k `elementOf` ns
				, not $ nach k `elementOf` ns 
				]
			  , and [       nach k `elementOf` ns
				, not $ von  k `elementOf` ns 
				]
			  ]
			) $ lkanten g


-- | bisektionsweite: minimale anzahl kanten die zur bisektion
-- | entfernt werden muss -> maß für zuverlässigkeit von netzen

bisektionsweite :: GraphC a => Graph a -> Int
bisektionsweite = cardinality . head . bisektionen
