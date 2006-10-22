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

import qualified Autolib.Reporter.Set
import Data.Typeable

data Graceful = Graceful deriving ( Eq, Ord, Show, Read, Typeable )

instance ( GraphC a , Show a )
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
	    s2 = ( text "domain (f)", mkSet $ keysFM fm )
        Autolib.Reporter.Set.subeq s1 s2

        let f x = lookupWithDefaultFM fm (error "Graceful") x
        peng $ gmap ( \ v -> ( v, f v ) ) g
	     { layout_hints = [ "-Nshape=ellipse" ] }
		
	let s3 = ( text "range (f)", mkSet $ eltsFM fm )
	    m4 = mkSet [ 0 .. cardinality $ kanten g ]
	    s4 = ( text "{ 0, 1 .. |E(G)| }" , m4 )
        Autolib.Reporter.Set.eq s3 s4
 
    total p g fm = do
        let range = mkSet $ eltsFM fm
	assert ( cardinality range == cardinality ( knoten g ))
	       ( text "ist  f  injektiv?" )
	let m1 = mkSet [ 1 .. cardinality $ kanten g ]
	    s1 = ( text "{ 1 .. |E(G)| }", m1 )
            f x = lookupWithDefaultFM fm (error "Graceful") x
            s3 = ( text "{ abs (f(x) - f(y)) | xy in E(G) }"
		 , smap ( \ k -> abs ( f (von k) - f (nach k) ) )
		   $ kanten g
		 )
        Autolib.Reporter.Set.eq s1 s3
	inform $ text "Das ist ein graceful labeling."
	    
instance GraphC a 
      => C.Measure Graceful ( Graph a ) ( FiniteMap a Int ) where
    measure p g fm = 1

make :: Make
make = direct Graceful $ bintree 12

-------------------------------------------------------------------------

bintree :: Int -> Graph Int
bintree n = mkGraph 
   ( mkSet [ 1 .. n ] )
   ( mkSet $ do k <- [ 2 .. n ] ; return $ kante k (k `div` 2) )
     




    
   

