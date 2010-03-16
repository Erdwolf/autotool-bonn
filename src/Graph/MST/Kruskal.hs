module Graph.MST.Kruskal where

import qualified Graph.MST.DSF as DSF

import Autolib.Graph.Graph

import Data.Map ( Map )
import qualified Data.Map as M

import Autolib.Util.Sort
import Control.Monad ( forM )

-- | compute weight of a minimal spanning forest
weight :: ( GraphC v, Num w, Ord w )
       => ( Graph v, Map ( Kante v ) w )
       -> w
weight ( g, w ) = DSF.run $ do
    wss <- forM ( sortBy snd $ M.toList w ) $ \ (k,w) -> do
        linked <- DSF.join ( von k ) ( nach k )
        return [ w | linked ]
    return $ sum $ concat wss

