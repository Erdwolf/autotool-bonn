module Graph.Selfcom where

--  $Id$

import Autolib.Graph.Type
import Autolib.Graph.Ops ( complement )
import Autolib.Graph.Basic

import Inter.Types
import Autolib.Reporter
import Autolib.Reporter.Subset
import Autolib.ToDoc
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import Data.Typeable

data Selfcom = Selfcom deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Selfcom Int ( Graph Int, FiniteMap Int Int ) where

    describe p i = vcat
        [ text "Gesucht ist ein regulärer selbstkomplementärer Graph"
	, text "mit wenigstens" <+> toDoc i <+> text "Knoten."
	, text "Sie sollen auch die passende Isomorphie angeben."
	]

    initial p i = ( circle [ 1 .. i ]
		  , listToFM $ zip [ 1 .. i ] $ reverse [ 1 .. i ]
		  )
    
    total p i (g, f) = do
        inform $ vcat [ text "Der Graph ist" , nest 4 $ toDoc g ]
        let h = complement g
        inform $ vcat [ text "Das Komplement ist" , nest 4 $ toDoc h ]
	check_iso f g h
        inform $ text "Dieser Graph ist selbstkomplementär."
	check_reg g
        assert ( size g >= i )
	       $ text "Ist der Graph groß genug?"

instance Size ( Graph Int, FiniteMap Int Int ) where 
    size ( g, f ) = size g

make :: Make
make = direct Selfcom ( 6 :: Int )

--------------------------------------------------------------------------

check_iso :: ( GraphC a, GraphC b )
	  => FiniteMap a b
	  -> Graph a
	  -> Graph b
	  -> Reporter ()
check_iso f g h = do
    inform $ vcat [ text "Sie behaupten diese Isomorphie" 
		  , nest 4 $ text "f" <+> equals <+> toDoc f
		  , text "von"
		  , nest 4 $ info g
		  , text "nach"
		  , nest 4 $ info h
		  ]
    equal_set ( text "V" <> parens ( info g ) , knoten g )
              ( text "domain f"               , mkSet $ keysFM f )
    equal_set ( text "V" <> parens ( info h ) , knoten h )
              ( text "range f"                , mkSet $ eltsFM f )
    fromto ( text "f" , f ) g h
    let invert fm = listToFM $ map ( \ (x,y) -> (y,x) ) $ fmToList fm
    fromto ( text "f^-", invert f ) h g        
    inform $ text "Das ist wirklich eine Isomorphie."

fromto :: ( GraphC a, GraphC b )
       => ( Doc, FiniteMap a b )
       -> Graph a
       -> Graph b
       -> Reporter ()
fromto ( doc, f ) g h = do
    inform $ vcat
	   [ text "bildet" <+> doc 
	   , text "jede Kante von" <+> info g
	   , text "auf eine Kante von" <+> info h
	   , text "ab?"
	   ]
    let fun x = lookupWithDefaultFM f (error "Selfcom.fromto") x
    let wrong = do
            k <- lkanten g
	    let k' = kante (fun $ von k) (fun $ nach k)
            guard $ not $ k' `elementOf` kanten h
	    return $ fsep [ text "das Bild", toDoc k' , text "von", toDoc k
			  , text "in", text "E" <> parens (info g)
			  , text "ist nicht in", text "E" <> parens (info h)
			  ]
    if not $ null wrong 
       then reject $ text "nein:" <+> vcat wrong
       else return ()

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
           , text "überein?"
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
    inform $ fsep [ text "ist der Graph", info g, text "regulär?" ]
    let degs = fmToList $ addListToFM_C union emptyFM $ do
           x <- lknoten g
	   return (degree g x, unitSet x)
    if length degs > 1
       then reject $ text "nein, die Knotengrade sind" $$ toDoc degs
       else inform $ text "ja."


