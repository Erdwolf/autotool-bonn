module Graph.Bi.Quiz where

--  $Id$

import Autolib.Graph.Type
import Graph.Bi.Plain
import Graph.Bi.Config

import Inter.Quiz
import Inter.Types
import System.Random
import Control.Monad

import Autolib.Graph.Type
import Autolib.Set

instance Generator Bi Config ( Graph Int , Set Int ) where
    generator _ conf key = do

       let ns = [ 1 .. nodes conf ]

       froms <- n_of (div (nodes conf) (teil conf)) ns

       let tos = filter (not. flip elem froms) ns

       ks <- mapM ( \ _ -> do from <- one_of froms
		              to   <- one_of tos
		              return $ kante from to
		  ) [ 1 .. edges conf ]

       return ( mkGraph (mkSet ns) (mkSet ks) 
	      , mkSet froms
	      )

instance Project Bi ( Graph Int, Set Int ) ( Graph Int ) where project _ = fst

make :: Make
make = quiz Bi rc

----------------------------------------------------------------------------------------------------

one_of :: [a] -> IO a
one_of xs = randomRIO ( 0 , pred $ length xs ) >>= return . (!!) xs

n_of :: Eq a => Int -> [a] -> IO [a]
n_of 0 _  = return []
n_of n xs = do x <- one_of xs
	       ys <- n_of (pred n) $ filter ( /= x ) xs
	       return $ x : ys
