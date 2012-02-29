{-# LANGUAGE NoMonomorphismRestriction #-}
module AVLBaumBonn.GraphViz (toPng) where

import System.IO.Unsafe (unsafePerformIO)
import Text.XHtml (showHtml, Html, image, (!), src, alt, anchor, href)
import Data.GraphViz  --(runGraphviz, GraphvizCommand(Dot), setDirectedness, graphToDot, nonClusteredParams, fmtNode, fmtEdge, toLabel, GraphvizOutput(Png))
import Data.GraphViz.Attributes.HTML (HtmlTextItem)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types (GlobalAttributes(GraphAttrs))
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>),(<.>))
import System.IO (openTempFile, hClose)
import Control.Concurrent (threadDelay)
import Data.Hashable (Hashable(hash))
import qualified Data.Tree as T

import Hex (hex)

picsDir = ".."</>"pics"

toTree t = T.unfoldTree uf t
   where
       uf t |       isLeaf t       = (Nothing,[])
            | isLeaf l && isLeaf r = (Just k,[])
            |             isLeaf r = (Just k,[l,r])
            | isLeaf l             = (Just k,[l,r])
            |       otherwise      = (Just k,[l,r])
        where k = key t
              l = left t
              r = right t

instance Hash (Baum.AVL.Type.AVLTree Int) where
    hash = hash . toTree

toDot avltree = graphElemsToDot params nodes edges
  where
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                , globalAttributes =
                                    [ GraphAttrs [Ordering "out"] -- child nodes are drawn in edge-order
                                    , NodeAttrs [Shape "plaintext"]
                                    ]
                                , isDirected = True
                                }
    numbered =
      flip evalState 0 $ flip traverse (toTree avltree) $ \x -> do
        i <- get; put (succ i)
        return (i, x)

    nodes =
      flip map (T.flatten numbered) $ \(i, x) -> (i, maybe "" show x)

    edges = do
        src@(T.Node (i, _) _) <- subtrees numbered
        dst@(T.Node (j, x) _) <- T.subForest src
        return (i, j, maybe (Just "white") (\_->Nothing) x)

    subtrees t = t : concatMap subtrees (T.subForest t)


toPng tree = unsafePerformIO $ do
   let fname = (hex $ fromIntegral $ hash graph) ++ ".png"
   runGraphviz (toDot tree)
               Png
               (picsDir </> fname)
   return $ anchor ! [ href ("../pics/" ++ fname) ]
          $ image ! [ src ("../pics/thumb_" ++ fname), alt ("AVL-Baum: " ++ show tree) ]
