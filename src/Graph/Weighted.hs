module Graph.Weighted 

( Kante (..)
, Graph (..)
, extract
, example
)

where

import Graph.Weighted.Data

import qualified Autolib.Graph.Graph as G

import Data.Map  ( Map )
import qualified Data.Map as M
import qualified Data.Set as S

example :: Int -> Graph Int Int
example n = 
    let v = S.fromList [ 0 .. n - 1 ]
        e = S.fromList $ do
          x <- S.toList v
          d <- takeWhile ( <= div n 2 ) $ map succ $ map (2^) [0 .. ]
          let y = (x + d) `mod` n
          return $ Kante { von = min x y, nach = max x y, gewicht = 1 + x + y }
    in  Graph { knoten = v, kanten = e }


extract :: G.GraphC v
        => Graph v w 
        -> ( G.Graph v, Map ( G.Kante v ) w )
extract g = 
    let weights = M.fromList $ do
            k <- S.toList $ kanten g
            return ( G.kante ( von k ) ( nach k ) , gewicht k )
    in  ( G.mkGraph ( knoten g ) ( M.keysSet weights ) , weights )

