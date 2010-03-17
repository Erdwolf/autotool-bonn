module Graph.Weighted 

( Kante (..)
, Graph (..)
, extract
, example
)

where

import Graph.Weighted.Data
import Graph.Weighted.Dot () -- instance

import qualified Autolib.Graph.Graph as G
import Autolib.Dot

import Data.Map  ( Map )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( fromMaybe )

example :: Int -> Graph Int Int
example n = 
    let v = S.fromList [ 0 .. n - 1 ]
        e = S.fromList $ do
          x <- S.toList v
          d <- takeWhile ( <= div n 2 ) $ map succ $ map (2^) [0 .. ]
          let y = (x + d) `mod` n
          return $ Kante { von = min x y, nach = max x y, gewicht = 1 + x + y }
    in  Graph { knoten = v, kanten = e }

instance ( G.GraphC v, Show w ) => ToDot ( Graph v w ) where
    toDot wg = let ( g, w ) = extract wg
                   weight k = fromMaybe ( error "no weight" ) 
                            $ M.lookup k w
               in  toDot ( g, weight )

extract :: G.GraphC v
        => Graph v w 
        -> ( G.Graph v, Map ( G.Kante v ) w )
extract g = 
    let weights = M.fromList $ do
            k <- S.toList $ kanten g
            return ( G.kante ( von k ) ( nach k ) , gewicht k )
    in  ( G.mkGraph ( knoten g ) ( M.keysSet weights ) , weights )

