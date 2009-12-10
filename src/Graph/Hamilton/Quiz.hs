module Graph.Hamilton.Quiz where

import Autolib.Graph.Type
import Autolib.Graph.Ops
import Autolib.Graph.Basic
import Autolib.Set
import Autolib.Util.Zufall ( genau , permutation )

import Graph.Hamilton.Plain
import Graph.Hamilton.Config

import Inter.Quiz
import Inter.Types

instance Generator Hamilton Config ( Graph Int, [Int] ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes conf ]
        
       kreis <- permutation ns
       let g = circle kreis

       zusatz <- 
           sequence $ replicate (edges conf - nodes conf) $ do
               [ from, to ] <- genau 2 kreis
	       return $ kante from to

       return ( links g zusatz
	      , kreis
	      )

instance Project Hamilton ( Graph Int, [Int] ) (Graph Int) 
    where 
        project _ = fst

make :: Make
make = quiz Hamilton Graph.Hamilton.Config.example

