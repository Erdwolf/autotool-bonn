-- | Layout routine for Data.Tree
--  $Id$

module Tree.Dot

where

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge

import Autolib.Boxing.Position

import Autolib.FiniteMap
import Data.Tree
import Data.Maybe
import Control.Monad ( guard )
import Control.Monad.State


make t = 
	let it = number t
	in  Autolib.Dot.Graph.Type
            { Autolib.Dot.Graph.directed = True
            , Autolib.Dot.Graph.name = "foo"
            , Autolib.Dot.Graph.nodes = nodes it
            , Autolib.Dot.Graph.edges = edges it
            , Autolib.Dot.Graph.attributes = []
            }
	    
-------------------------------------------------------------------------

number :: Tree a -> Tree ( Int, a )
number t = evalState ( num t ) 0

num :: Tree a -> State Int ( Tree ( Int, a) )
num ( Node f args ) = do
    i <- get
    put $ succ i
    args' <- mapM num args
    return $ Node (i, f) args'

subtrees :: Tree a -> [ Tree a ]
subtrees t @ ( Node f args ) = t : concat ( map subtrees args )

-------------------------------------------------------------------------

nodes :: Tree ( Int, String ) 
      -> [ Autolib.Dot.Node.Type ]
nodes t = do
    ( i, cs ) <- flatten t
    return $ Autolib.Dot.Node.blank
           { Autolib.Dot.Node.ident = show i
           , Autolib.Dot.Node.label = return cs
           , Autolib.Dot.Node.shape = return "plaintext"
           }  

edges :: Tree ( Int, a ) 
      -> [ Autolib.Dot.Edge.Type ]
edges t = do
    Node (i, f) jxs <- subtrees t
    Node (j, x) args <- jxs
    return $ Autolib.Dot.Edge.blank
           { Autolib.Dot.Edge.from  = show i
           , Autolib.Dot.Edge.to    = show j
	   , Autolib.Dot.Edge.directed = True
           }



