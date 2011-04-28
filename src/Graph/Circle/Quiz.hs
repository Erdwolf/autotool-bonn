{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graph.Circle.Quiz where

--  $Id$

import Autolib.Graph.Type
import Autolib.Set
import Autolib.Util.Zufall ( eins , genau )

import Graph.Circle.Plain
import Graph.Circle.Config

import Inter.Quiz
import Inter.Types

instance Generator Circle Config ( (Int,Graph Int) , Set Int ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes_complete conf ]
        
       cs <- genau (nodes_circle conf) ns

       let rest = filter ( not . flip elem cs ) ns

       -- kanten zwischen genau n knoten
       let edges_cs = map (uncurry kante) $ zip cs (tail $ cycle cs)

       -- noch mehr kanten raten
       edges_rest <- sequence $ replicate (edges conf) $ do
		     from <- eins cs
		     to <- eins rest
		     return $ kante from to

       return ( ( nodes_circle conf
		, mkGraph (mkSet ns) (mkSet $ edges_cs ++ edges_rest) 
		)
	      , mkSet cs
	      )

instance Project Circle ( (Int,Graph Int), Set Int ) (Int,Graph Int) where 
    project _ = fst

make :: Make
make = quiz Circle rc

