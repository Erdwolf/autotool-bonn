module Graph.Col.Quiz where

--  $Id$

import Autolib.Graph.Type
import Graph.Util
import Graph.Color
import Graph.Col.Plain
import Graph.Col.Config

import Inter.Quiz
import Inter.Types
import System.Random
import Control.Monad

import Autolib.Graph.Type
import Autolib.FiniteMap

instance Generator Col Config ( ( Integer, Graph Int) , FiniteMap Int Color ) where
    generator p conf key = do
       let vs  = mkSet [ 1 .. nodes conf ]
       -- erstmal die knoten irgendwie färben
       vcs <- mapM ( \ v -> do
           c <- randomRIO ( 0, fromIntegral (chi conf) - 1 )
	   return ( v, toEnum c )
         ) $ setToList vs
       let f = listToFM vcs
           col = lookupWithDefaultFM f (error "Graph.Quiz")
       -- dann (mehr als) genug kanten raten
       ks <- sequence $ replicate ( nodes conf * edges conf ) $ do 
           x <- randomRIO ( 1, nodes conf )
           y <- randomRIO ( 1, nodes conf )
           return (kante x y)
       let oks = take ( edges conf ) $ do 
	     k <- ks
	     guard $ col (von k) /= col (nach k)
	     return k
       return ( ( fromIntegral $ chi conf
		, mkGraph vs ( mkSet oks ) 
		)
	      , f
	      )

instance Project Col  ( (Integer, Graph Int), FiniteMap Int Color ) 
                      ( (Integer, Graph Int) ) where
   project p ( (c, g), zs ) = (c, g)

make :: Make
make = quiz Col Graph.Col.Config.rc





