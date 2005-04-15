module Graph.Way.Quiz where

--  $Id$

import Graph.Way.Plain ( Way ( Way ) )
import Graph.Way.Config ( Config , nodes , edges , rc )

import Autolib.Util.Zufall ( eins )

import Autolib.Graph.Type ( Graph , mkGraph , kante )
import Autolib.Graph.Adj ( wegematrix )
import Autolib.Set ( mkSet )

import Data.Array ( elems )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

instance Generator Way Config ( (Int,[Integer]) , Graph Int ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes conf ]

       ks <- sequence $ replicate (edges conf) $ do
	     x <- eins ns
	     y <- eins ns
	     return $ kante x y

       let g = mkGraph (mkSet ns) (mkSet ks)

       return ( (nodes conf,elems $ wegematrix g) , g )

instance Project Way ((Int,[Integer]),Graph Int) (Int,[Integer]) where 
    project _ = fst

make :: Make
make = quiz Way rc
