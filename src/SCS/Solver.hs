import LCS.Code 

import Autolib.Genetic 
import Autolib.Genetic.Operator

import Autolib.Util.Zufall

import System.Environment
import Data.List ( nub )
import Control.Monad ( forM_ )

for = flip map

main = do
    subs <- getArgs
    let sigma = nub $ concat subs
        goal = length $ concat subs
    evolve $ Config 
          { fitness = \ w -> negate $ 
               length w 
             + 10 * sum ( for subs $ \ s -> 
                   let d =  length s - length ( lcs s w ) 
                   in  d
                  )
          , threshold = 0
	  , present = \ kvs -> forM_ ( take 3 kvs ) print
	  , trace   = \ kvs -> forM_ ( take 3 kvs ) $ \ (k,v) -> do
                        print ( length v, v, k )
	  , size    = 20
	  , generate = someIO sigma $ length subs
	  , combine = \ x y -> do
                action <- eins [ onepoint_varying x y, twopoint_varying x y ]
                action
          , num_combine = undefined
	  , mutate = \ w -> do
               action <- eins [ chop w, insert sigma w ]
               action
          , scheme = Tournament 5
          , num_mutate = undefined 
          , num_compact =  undefined
          , num_steps = Nothing
          , num_parallel = 1
          }

chop w =  do
                i <- randomRIO ( 0, length w )
                j <- randomRIO ( 0, length w )
                let (premid, post) = splitAt ( max i j ) w
                    (pre, mid) = splitAt (min i j) premid
                return $ pre ++ post

insert sigma w = do
    i <- randomRIO ( 0, length w )
    let ( pre, post ) = splitAt i w
    x <- eins sigma
    return $ pre ++ x : post
