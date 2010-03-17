{-# language MultiParamTypeClasses, FlexibleInstances #-}

module Graph.TSP.Quiz where

import Graph.TSP.Plain ( TSP ( TSP ), matrix )
import Graph.TSP.Tropic
import Graph.TSP.Search 

import Graph.MST.Config 
import Graph.MST.Plain ( MST (MST) )
import qualified Graph.MST.Quiz


import Graph.Weighted
import Autolib.Util.Zufall
import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

import Control.Monad ( forM )
import Data.List ( tails )
import qualified Data.Set as S

-- | FIXME: das Würfeln (genauer: das Testen) kann lange dauern. 
-- Mehr als 10 Knoten sind nicht zu empfehlen.

instance Generator TSP Config ( Tropic Int, Graph Int Int, [Int] ) where
    generator TSP conf key = do
        (wg, bs) <- ( do wg <- generator MST conf key ; return ( wg, search $ matrix wg ) )
                `repeat_until` \ ( wg, bs ) -> not $ null bs
        let s = if nodes conf > 10 then head bs else last bs
        return ( weight s, wg, tail $ path s )

instance Project TSP ( Tropic Int, Graph Int Int, [ Int ] ) ( Tropic Int, Graph Int Int )  where 
    project TSP (w,g, p) = (w,g)

make :: Make
make = quiz TSP $ Config { nodes = 10 , edges = 20, weight_bounds = (2, 20) }
