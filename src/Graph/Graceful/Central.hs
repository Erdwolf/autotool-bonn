module Graph.Graceful.Central where

--  $Id$

import Graph.Util
import Graph.Color

import Graph.Cage.Config

import Autolib.Graph.Ops ( gmap )
import Autolib.Dot ( peng, Layout_Program (..) )

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Util.Sort
import qualified Challenger as C

import qualified Autolib.Reporter.Subset
import Data.Typeable

data Graceful = Graceful deriving ( Eq, Ord, Show, Read, Typeable )

instance GraphC a 
    => C.Partial Graceful ( Graph a ) ( FiniteMap a Int ) where

    report p g = do
        inform $ vcat
	       [ text "Gesucht ist ein graceful labeling für"
	       , nest 4 $ toDoc g
	       ]
        peng g

    initial p g = listToFM $ zip (lknoten g) [ 0 .. ]

    partial p g fm = do
        let s1 = ( text "V(G)", knoten g )
	    s2 = ( text "Urbildmenge des labeling", mkSet $ keysFM fm )
        Autolib.Reporter.Eqset.check s1 s2
	let s3 = ( text "Bildmenge des labeling", mkSet $ eltsFM fm )
	    m4 = mkSet [ 0 .. cardinality $ kanten g ]
	    s4 = ( text "{ 0, 1 .. |E(G)| }" , m4 )
        Autolib.Reporter.Subset.check s3 s4
 
    total p g fm = do
        let range = mkSet $ eltsFM fm
	assert ( cardinality range == cardinality $ knoten g )
	       ( text "ist das labeling injektiv?" )
        Autolib.Reporter.Subset.check
	    -- ist Teilmenge von  [ 0 .. cardinality $ kanten g ]
        

	inform $ text "OK."
	    

instance  C.Measure Cage Config ( Graph Int) where
    measure p conf g = fromIntegral $ size g

make :: Make
make = direct Cage Graph.Cage.Config.rc

-------------------------------------------------------------------------

-- | all colourings with at most c colours
colourings :: GraphC a
	   => Int -> Graph a 
           -> [ FiniteMap a Int ]
colourings c g = list_colourings g 
    ( listToFM $ do v <- lknoten g ; return (v, mkSet [ 1 .. c ] ) )
    ( emptyFM )

list_colourings :: GraphC a
     => Graph a
     -> FiniteMap a ( Set Int ) -- ^ noch mögliche farben für knoten
     -> FiniteMap a Int -- ^ bereits gefärbt
     -> [ FiniteMap a Int ] --  ^ resultate
list_colourings g cons done | 0 == sizeFM cons  = return done
list_colourings g cons done = do 
    let (v, cs) : rest = sortBy ( \ (v, cs) -> cardinality cs ) 
	               $ fmToList cons
    this <- setToList cs
    let handle n cons = 
	    let cs = lookupset cons n
	    in  updateFM cons n ( cs `minusSet` unitSet this )
	cons' = foldr handle cons $ lnachbarn g v
    list_colourings g ( delFromFM cons' v ) ( addToFM done v this )

updateFM :: Ord a => FiniteMap a b -> a -> b -> FiniteMap a b
updateFM fm a b = case lookupFM fm a of
    Just _ -> addToFM fm a b
    Nothing -> fm

-------------------------------------------------------------------------

-- | all circles on length less than l
small_circles :: GraphC a
	      => Int 
	      -> Graph a 
	      -> [[a]]
small_circles l g = do
    k <- lkanten g
    let x = von k; y = nach k
    z <- lnachbarn g $ y
    guard $ x /= z
    p <- paths l g [ z, y, x ]
    guard $ head p == last p
    return p

paths l g path = path : do
    guard $ length path <= l
    x <- lnachbarn g $ head path
    paths (pred l) g $ x : path





    
   

