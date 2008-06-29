module Pentomino.Find where

import Pentomino.Cover
import qualified Autolib.Genetic as G

import Autolib.ToDoc

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import System.Random
import Data.Ix

main = G.evolve conf

conf = G.Config 
    { G.fitness = \ f -> 
          ( negate $ overlaps f
          , reach f
          , unreach f 
          )
    , G.threshold = ( 0, 60, 130 + 5 * 12 )
    , G.present = mapM_ ( \ (v,f) -> print (toDoc v <+> form f ) )
                . reverse . take 3
    , G.trace = print . map fst . take 10 
    , G.size  = 500
    , G.generate = roll
    , G.combine = \ f g -> fmap figure $ sequence $ do
          k <- [ 0 .. length ( pieces f ) - 1 ]
          return $ do
              s <- randomRIO ( False, True )
              return $ pieces ( if s then f else g ) !! k
    , G.num_combine = 200
    , G.mutate = \ f -> fmap figure $ sequence $ do
          p <- pieces f
          return $ do
              m <- randomRIO ( 0, length ( pieces f )) 
              if ( m > 1 ) 
                 then return p
                 else modify p
    , G.num_mutate = 200
    , G.num_compact = 10
    , G.num_steps = Nothing
    , G.num_parallel = 1
    }




-- | penalty for overlapping figures
overlaps :: Figure -> Int
overlaps f = 
    let m = M.fromListWith S.union $ do
              (k, c) <- zip [ 0 .. ] $ covers f
              x <- S.toList c
              return ( x, S.singleton k )
    in  sum $ filter ( > 1 ) 
            $ map S.size
            $ M.elems m

