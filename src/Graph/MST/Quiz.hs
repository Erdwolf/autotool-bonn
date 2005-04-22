module Graph.MST.Quiz where

--  $Id$

import Graph.MST.Plain ( MST ( MST ) , W , wfun )
import Graph.MST.Config ( Config , nodes , edges , weight_type , rc 
			, Weight ( Sum , Product , Random ) 
			)

import Autolib.Util.Zufall ( eins , permutation )
import Autolib.FiniteMap ( listToFM )

import Autolib.Graph.Type ( Graph , mkGraph , kante , Kante (..) )
import Autolib.Graph.SpanTree ( mst , weight )
import Autolib.Set ( mkSet )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

import System.Random ( randomRIO )

instance Generator MST Config ( (Int,Graph Int,W) , (Int,Graph Int) ) where
    generator _ conf _ = do

       let ns = [ 1 .. nodes conf ]

       perm <- permutation ns

       -- | ks0 sichert den zusammenhang des graphen
       let ks0 = map (uncurry kante) $ zip perm (tail perm)

       ks <- sequence $ replicate (edges conf) $ do
	     x <- eins ns
	     y <- eins ns
	     return $ kante x y

       let g = mkGraph (mkSet ns) (mkSet $ ks ++ ks0)

       ws <- mapM ( \ k@(Kante {von=v,nach=n}) -> case weight_type conf of
		    Sum -> return (k,v+n)
		    Product -> return (k,v*n)
		    Random x -> randomRIO ( 1 , x ) >>= \ r -> return (k,r)
		  ) $ ks ++ ks0       

       let w = listToFM ws

       let st = mst g (wfun w)
   
       let wmin = weight st (wfun w)

       return ( ( wmin , g , w )
	      , ( wmin , st )
	      )

instance Project MST ((Int,Graph Int,W),(Int,Graph Int)) (Int,Graph Int,W) where 
    project _ = fst

make :: Make
make = quiz MST rc
