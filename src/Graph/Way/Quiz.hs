module Graph.Way.Quiz where

--  $Id$

import Graph.Way.Plain ( Way ( Way ) )
import Graph.Way.Config ( Config , nodes , edges , rc )
import Graph.Way.Input ( Input (..) , Solvability ( Solvable ) )

import Autolib.Util.Zufall ( eins )

import Autolib.Graph.Type ( Graph , mkGraph , kante )
import Autolib.Graph.Adj ( wegematrix , zeilen )
import Autolib.Set ( mkSet )

import Data.Array ( elems )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

instance Generator Way Config ( Input , Maybe (Graph Int) ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes conf ]

       ks <- sequence $ replicate (edges conf) $ do
	     x <- eins ns
	     y <- eins ns
	     return $ kante x y

       let g = mkGraph (mkSet ns) (mkSet ks)

       return ( Input (zeilen $ wegematrix g) Solvable , Just g )

instance Project Way (Input,Maybe (Graph Int)) Input where 
    project _ = fst

make :: Make
make = quiz Way rc
