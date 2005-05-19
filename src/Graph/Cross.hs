module Graph.Cross where

--  $Id$

import Graph.Util
import Autolib.Graph.Basic
import Autolib.Dot

import Autolib.Hash
import Autolib.FiniteMap
import Autolib.Boxing.Position
import Inter.Types
import Autolib.Reporter
import qualified Challenger as C

import Data.Typeable
import Data.Maybe ( fromMaybe )
import Data.List ( tails )

type Punkt = ( Integer, Integer )
type Strecke = ( Punkt, Punkt )

-- | straight line drawing
type Karte a = FiniteMap a Punkt

instance GraphC a 
       => C.Measure Cross ( Int, Graph a ) ( Karte a ) where
    measure p (c, g) f = 
        1000 * fromIntegral (length $ crossings g f) + extension f

extension :: Karte a -> Integer
extension f = 
    let range :: [ Integer ] -> Integer
        range [] = 0
        range xs = maximum xs - minimum xs
        xys = eltsFM f
    in  max (range $ map fst xys)
	    (range $ map snd xys)

data Cross = Cross deriving ( Eq, Ord, Show, Read, Typeable )

instance ( Show a, GraphC a )
       => C.Partial Cross ( Int, Graph a ) ( Karte a ) where

    report p (c, g) = do
        inform $ vcat
	       [ text "Ordnen Sie den Knoten dieses Graphen"
	       , nest 4 $ toDoc g
	       ]
	peng g
	inform $ vcat 
	       [ text "ganzzahlige Koordinaten zu,"
	       , text "so daß sich eine Zeichnung mit höchstens"
                     <+> toDoc c <+> text "Kreuzungen"
	       , text "und mit geringer Ausdehnung ergibt."
               , parens $ text "Bewertung: 1000 * Kreuzungszahl + größte Ausdehnung"
	       ]

    initial p (c, g) = listToFM $ do
        (k, x) <- zip [ 0 .. ] $ lknoten g
	return (x, (k, k^2))

    partial p (c, g) b = do
        inform $ text "Haben Sie jedem Knoten einen Punkt zugeordnet?"
        equal_set ( text "V(G)"     , knoten g         )
                  ( text "domain(f)", mkSet $ keysFM b )

    total p (c, g) b = do
        inform $ text "Ihre Zeichnung ist:"
        peng $ Pin g b 
        let crs = crossings g b
        inform $ vcat
	       [ text "Ihre Zeichnung ergibt diese Kreuzungen:"
	       , nest 4 $ toDoc crs
	       ]
	when ( length crs > c ) $ reject 
	     $ text "Das sind zuviele."


make :: Make
make = direct Cross (1 :: Int, clique $ mkSet [1:: Int .. 5])

g :: Graph Int
g = clique $ mkSet [1 :: Int .. 5]

b :: Karte Int
b = C.initial Cross (1 :: Int, g)

--------------------------------------------------------------

data Pin a = Pin ( Graph a ) ( Karte a )
    deriving ( Eq, Ord, Show )

instance GraphC a  => Hash ( Pin a ) where
    hash (Pin g f) = hash (g, f)

instance ( GraphC a, Show a ) => ToDot ( Pin a ) where
    toDot ( Pin g f ) = toDot $ pin g f 
    toDotProgram _ = Neato
    toDotOptions _ = unwords [ "-s" ]

pin :: GraphC a
    => Graph a
    -> Karte a
    -> Graph a
pin = pinN 7

pinN :: GraphC a => Position -> Graph a -> Karte a -> Graph a
pinN sz g f =
    let 
        h = mapFM ( \ k v -> toPos v ) f
        ( ul, or ) = minimax $ eltsFM h
	scale = sz / abs ( or - ul )
    in  g { graph_layout = mapFM ( \ k p -> ( p - ul ) * scale ) h
	  , bounding = or
	  , show_labels = True
          }

toPos ( x , y ) = Position { width  = fromIntegral x 
				   , height = fromIntegral y
				   }
---------------------------------------------------------------

crossings :: GraphC a 
          => Graph a 
          -> Karte a 
          -> [(Kante a, Kante a)]
crossings g f = do
    let look x = fromMaybe (error "Graph.Cross.crossings.look") $ lookupFM f x
    e : es <- tails $ lkanten g
    let a = look $ von  e
        b = look $ nach e 
    e' <- es
    guard $ isEmptySet $ intersect    ( mkSet [ von e , nach e  ] )
                                      ( mkSet [ von e', nach e' ] )
    let a' = look $ von  e'
        b' = look $ nach e' 
    guard $ is_crossing (a, b) (a', b')
    return (e, e')

-- | strecken schneiden sich
is_crossing :: Strecke -> Strecke -> Bool
is_crossing ab cd = trennt ab cd && trennt cd ab 
   
-- | liegen auf verschiedenen Seiten der Geraden
trennt :: Strecke -> Strecke -> Bool
trennt (a, b) (c, d) = 0 <= area2 a c d * area2 b d c

-- | doppeltes des orientierten Flächeninhalts
area2 :: Punkt -> Punkt -> Punkt -> Integer
area2 (x1,x2) (y1,y2) (z1,z2) = 
    det3 [[x1, x2, 1], [y1, y2, 1], [z1, z2, 1]]

det3 :: Num a =>  [[a]] -> a
det3 [ [a1,a2,a3], [b1,b2,b3], [c1,c2,c3] ]
    = a1 * ( b2 * c3 - b3 * c2 )
    - a2 * ( b1 * c3 - b3 * c1 )
    + a3 * ( b1 * c2 - b2 * c1 )


