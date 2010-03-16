module Graph.MST.Quiz where

import Graph.MST.Plain ( MST ( MST ) )
import Graph.MST.Weight ( Weight , wfun , direct )
import Graph.MST.Config ( Config , nodes , edges , weight_type , rc )

import Autolib.Util.Zufall ( eins , permutation )

import Autolib.Graph.Type ( Graph , mkGraph , kante )
import Autolib.Graph.SpanTree ( mst , weight )
import Autolib.Set ( mkSet )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

instance Generator MST Config ( (Int,Graph Int,Weight) , (Int,Graph Int) ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes conf ]

       perm <- permutation ns

       --  ks0 sichert den zusammenhang des graphen
       let ks0 = map (uncurry kante) $ zip perm (tail perm)

       ks <- sequence $ replicate (edges conf) $ do
	     x <- eins ns
	     y <- eins ns
	     return $ kante x y

       let g = mkGraph (mkSet ns) (mkSet $ ks ++ ks0)

       let w_direct = direct (weight_type conf) $ ks ++ ks0

       let st = mst g (wfun w_direct)
   
       let wmin = weight st (wfun w_direct)

       return ( ( wmin , g , w_direct )
	      , ( wmin , st )
	      )

instance Project MST ((Int,Graph Int,Weight),(Int,Graph Int)) (Int,Graph Int,Weight) where 
    project _ = fst

make :: Make
make = quiz MST rc
