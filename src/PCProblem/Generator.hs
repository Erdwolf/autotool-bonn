module PCProblem.Generator

( generator 
)

where

-- -- $Id$

import PCProblem.Type
import PCProblem.Param
import PCProblem.Solver

import Util.Zufall
import Util.Wort
import Random
import Letters
import Maybe
import Data.Set

gen :: Param -> IO PCP
gen g = do
    uvs <- sequence $ do 
           k <- [ 1 .. paare g ]
	   return $ do
	       l <- randomRIO (1, breite g)
	       [u, v] <- sequence $ replicate 2 $ someIO ( alpha g ) l
	       return ( u, v )
    let (u1, v1) : (u2, v2) : rest = uvs
    -- hackerei für anfang und ende
    let uvs' = ( v1 ++ u1, v1 ) : ( u2 , v2 ++ u2 ) : rest
    xys <- permutation uvs'
    return $ PCP xys


sol :: Param -> IO ( PCP, Maybe Folge )
-- nur eine kürzeste lösung
sol g = do
    i <- gen g -- Instanz
    let fs = solutions ( viel g ) ( fern g ) i
    return ( i, listToMaybe $ take 1 $ fs )

triviale_instanz :: PCP -> Bool
-- falls ein buchstabe nur einseitige differenzen hat,
-- dann nützt er in der lösung nicht viel (?? - check)
triviale_instanz i @ ( PCP uvs ) = or $ do
    x <- setToList $ letters i
    let diffs = do 
	    ( u, v ) <- uvs
	    return ( count x u - count x v )
    return $ all ( >= 0 ) diffs || all ( <= 0 ) diffs

count :: Eq a => a -> [a] -> Int
count x = length . filter ( == x ) 

generator :: Param -> IO ( PCP, Folge )
-- erzeuge studentenfreundliche lösbare instanzen
generator g = do
    ( i, Just f ) <- sol g `repeat_until` \ ( p, mf ) -> 
        not ( triviale_instanz p ) && 
        case mf of
	     Nothing -> False
	     Just f  -> nah g <= length f && length f <= fern g
    return ( i, f )


