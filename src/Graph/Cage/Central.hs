module Graph.Cage.Central where

--  $Id$

import Graph.Util
import Graph.Color

import Graph.Cage.Config

import Autolib.Graph.Ops ( gmap )
import Autolib.Graph.Basic -- ( circle )
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

data Cage = Cage deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Cage Config ( Graph Int ) where

    report p conf = do
        inform $ vcat
	       [ text "Gesucht ist ein Graph mit"
	       , text "chromatischer Zahl >=" <+> toDoc ( chi conf )
	       , text "und Taillenweite >=" <+> toDoc ( girth conf )
	       ]

    initial p conf = circle [ 1 .. 6 ]

    partial p conf g = do
        inform $ vcat [ text "Der Graph ist" , nest 4 $ toDoc g ]
	peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" ]
		 }
	validate g
 
    total p conf g = do
        let gi   =  small_circles (girth conf - 1) g
	when ( not $ null gi ) $ reject $ vcat
	     [ text "Die Taillenweite ist <" <+> (toDoc $ girth conf) <+> text ","
	     , text "denn es gibt (unter anderem) diese kurzen Kreise:"
	     , nest 4 $ toDoc $ take 5 gi
	     ]
        let cols =  colourings (chi conf - 1) g
	when ( not $ null cols ) $ reject $ vcat
	     [ text "Die chromatische Zahl ist <" <+> (toDoc $ chi conf) <+> text ","
	     , text "denn es gibt (unter anderem) diese F�rbungen:"
	     , nest 4 $ toDoc $ take 5 cols
	     ]
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
     -> FiniteMap a ( Set Int ) -- ^ noch m�gliche farben f�r knoten
     -> FiniteMap a Int -- ^ bereits gef�rbt
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

-- | all circles of length at most l
small_circles :: GraphC a
	      => Int 
	      -> Graph a 
	      -> [[a]]
small_circles l g = do
    k <- lkanten g
    p <- paths l g [ von k, nach k ]
    guard $ length p > 2
    guard $ kante (head p) (last p) `elementOf` kanten g
    return p

paths l g path = path : do
    guard $ length path < l
    x <- lnachbarn g $ head path
    guard $ not $ x `elem` path
    paths l g $ x : path

------------------------------------------------------------------

g = mkGraph ( mkSet [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11] )
      ( mkSet [
		kante 1 2, kante 2 3, kante 3 4, kante 4 5, kante 5 1,
		kante 1 8, kante 1 11, kante 2 7, kante 2 9,
		kante 3 8, kante 3 10,
		kante 4 9, kante 4 11, kante 5 10, kante 5 7,
		kante 6 7, kante 6 8, kante 6 9, kante 6 10, kante 6 11
	       ]
      )




    
   

