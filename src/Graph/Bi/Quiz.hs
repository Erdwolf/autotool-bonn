module Graph.Bi.Quiz where

--  $Id$

import Autolib.Graph.Type
import Autolib.Util.Zufall ( eins , genau )
import Autolib.Set

import Graph.Bi.Plain
import Graph.Bi.Config

import Inter.Quiz
import Inter.Types

instance Generator Bi Config ( Graph Int , Set Int ) where
    generator _ conf key = do

       let ns = [ 1 .. nodes conf ]

       froms <- genau (div (nodes conf) (teil conf)) ns

       let tos = filter (not. flip elem froms) ns

       ks <- mapM ( \ _ -> do from <- eins froms
		              to   <- eins tos
		              return $ kante from to
		  ) [ 1 .. edges conf ]

       return ( mkGraph (mkSet ns) (mkSet ks) 
	      , mkSet froms
	      )

instance Project Bi ( Graph Int, Set Int ) ( Graph Int ) where project _ = fst

make :: Make
make = quiz Bi rc
