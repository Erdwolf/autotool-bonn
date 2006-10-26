{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module PCProblem.Generator

( generator 
)

where

import PCProblem.Type
import PCProblem.Param
import PCProblem.Solver

import Autolib.Util.Zufall
import Autolib.Util.Wort
import Autolib.Letters

import System.Random
import Data.Maybe
import Data.List ( nub )
import Autolib.Set

import Inter.Quiz

gen :: Param -> IO PCP
gen p = do
{- hier kamen noch doppelte paare und/oder paare mit identischen strings vor
    uvs <- sequence $ do 
           k <- [ 1 .. paare p ]
	   return $ do
	       l <- randomRIO (1, breite p)
	       [u, v] <- sequence $ replicate 2 $ someIO ( alpha g ) l
	       return ( u, v )
-}
    uvs <- ( sequence $ replicate ( paare p ) $ pair p )
	   `repeat_until` 
	   \ uvs -> and [ --  alle paare verschieden 
			  uvs == nub uvs

                          --  alle buchstaben kommen vor
			, let (l,r) = unzip uvs
			  in length ( nub $ concat $ l ++ r ) 
			     == 
			     length ( alpha p )

                          --  es gibt mindestens ein wort mit
                          --  (mindestens) der geforderten breite
                        , any ( \ (u,v) -> or [ length u >= breite p
					      , length v >= breite p 
					      ] 
			      ) uvs 
			]
    let (u1, v1) : (u2, v2) : rest = uvs
    -- hackerei für anfang und ende
    let uvs' = ( v1 ++ u1, v1 ) : ( u2 , v2 ++ u2 ) : rest
    xys <- permutation uvs'
    return $ PCP xys

pair :: Param -> IO (String,String)
pair p = ( do l <- randomRIO (1,breite p)
	      [u,v] <- sequence $ replicate 2 $ someIO ( alpha p ) l
	      return (u,v)
	 ) `repeat_until` ( uncurry (/=) )


-- | nur eine kürzeste lösung
sol :: Param -> IO ( PCP, Maybe Folge )
sol p = do
    i <- gen p -- Instanz
    let fs = solutions ( viel p ) ( fern p ) i
    return ( i, listToMaybe $ take 1 $ fs )

-- | falls ein buchstabe nur einseitige differenzen hat,
-- dann nützt er in der lösung nicht viel (?? - check)
triviale_instanz :: PCP -> Bool
triviale_instanz i @ ( PCP uvs ) = or $ do
    x <- setToList $ letters i
    let diffs = do 
	    ( u, v ) <- uvs
	    return ( count x u - count x v )
    return $ all ( >= 0 ) diffs || all ( <= 0 ) diffs

count :: Eq a => a -> [a] -> Int
count x = length . filter ( == x ) 

instance Generator PCProblem Param ( PCP, Folge ) where
   generator _ conf key = do
       ( i, Just f ) <- sol conf `repeat_until` \ ( i, mf ) -> 
           not ( triviale_instanz i ) && 
           case mf of
	       Nothing -> False
	       Just f  -> and [ nah conf <= length f 
			      , length f <= fern conf
                                -- alle paare sollen in der lösung vorkommen
			      , length (nub f) == paare conf
			      ]
       return ( i, f )

instance Project PCProblem ( PCP, Folge ) PCP where
   project p ( i, f ) = i



