module Graph.Cage.Central where

--  $Id$

import Graph.Util
import Graph.Color

import Autolib.Graph.Ops ( gmap )
import Autolib.Graph.Basic ( circle )
import Autolib.Dot ( peng, Dot )

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
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
	     [ text "Die Taillenweite ist <" <+> (girth conf) <+> text ","
	     , text "denn es gibt diese kurzen Kreise:"
	     , nest 4 $ take 5 $ toDoc gi
	     ]
        let cols =  colourings (chi conf - 1) g
	when ( not $ null cols ) $ reject $ vcat
	     [ text "Die chromatische Zahl ist <" <+> (girth conf) <+> text ","
	     , text "denn es gibt diese Färbungen:"
	     , nest 4 $ take 5 $ toDoc cols
	     ]
	inform $ text "OK."
	    

instance  C.Measure Cage Conf ( Graph Int) where
    measure p conf g = size g

make :: Make
make = direct Cage Graph.Cage.Config.rc

---------------------------------------------------------------------------

colourings :: GraphC a
	   => Int -> Graph a 
           -> [ FiniteMap a Int ]
colourings c g = cols c g 
    ( listToFM $ do v <- lknoten g ; return (v, mkSet [ 1 .. c ] ) )
    ( emptFM )

cols c g cons done | 0 == sizeFM cons  = return done

cols c g cons done = do 
    let (v, cs) : rest = sortBy ( \ (v, cs) -> cardinality cs ) 
	               $ fmToList cons
    this <- setToList cs
    let handle n cons = 
	    let cs = lookupset cons n
	    in  insertFM cons n ( cs `minusSet` mkSet this )
	cons' = foldr handle cons $ nachbarn g v
    cols c g ( delFromFM cons' v ) ( insertFM done v this )

----------------------------------------------------------------------------

small_circles l g = do
    k <- lkanten g
    z <- nachbarn g $ nach k
    guard $ z /= von k
    p <- paths l g [ von k, nach k, z ]
    guard $ head p == last p
    return p

paths l g path = path : do
    guard $ length path < l
    x <- nachbarn g $ head path
    paths (pred l) g $ x : path





    
   

