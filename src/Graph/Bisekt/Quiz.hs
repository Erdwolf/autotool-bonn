module Graph.Bisekt.Quiz where

--  $Id$

import Autolib.Graph.Type
import Autolib.Util.Zufall ( eins , genau )
import Autolib.Set

import Graph.Bisekt.Plain
import Graph.Bisekt.Config

import Inter.Quiz
import Inter.Types

instance Generator Bisect Config ( (Int,Graph Int) 
				 , (Set (Kante Int),Set Int,Set Int )
				 ) where
    generator _ conf key = do

       let ns = [ 1 .. nodes conf ]

       links <- genau (div (nodes conf) 2) ns

       let rechts = filter (not. flip elem links) ns

       kanten_bisekt <- genau (edges_bisect conf) $ do
			l <- links
			r <- rechts
			return $ kante l r

       let k = div (edges conf ) 2
       let kanten m = sequence $ replicate k $ do
		      x <- eins m ; y <- eins m
		      return $ kante x y

       kanten_links <- kanten links
       kanten_rechts <- kanten rechts

       let ks = kanten_bisekt ++ kanten_links ++ kanten_rechts

       return ( ( edges_bisect conf
		, mkGraph (mkSet ns) (mkSet ks) 
		)
	      , ( mkSet kanten_bisekt
		, mkSet links
		, mkSet rechts
		)
	      )

instance Project Bisect ( (Int,Graph Int), (Set (Kante Int),Set Int,Set Int) ) 
                          (Int,Graph Int) where project _ = fst

make :: Make
make = quiz Bisect rc
