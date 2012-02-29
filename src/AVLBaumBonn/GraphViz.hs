{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, FlexibleInstances #-}
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
import Data.Traversable (traverse)
import Control.Monad.State (evalState, get, put)
import Baum.AVL.Type (AVLTree)

import AVLBaumBonn.Conversion (toTree, bonnifyTree)

import Hex (hex)

picsDir = ".."</>"pics"

instance Hashable a => Hashable (T.Tree a) where
    hash = hash . T.levels

instance Hashable (Baum.AVL.Type.AVLTree Int) where
    hash = hash . toTree


toDot :: AVLTree Int -> DotGraph Int
toDot avltree = graphElemsToDot params nodes edges
  where
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , fmtEdge = \ (_, _,n) -> maybe [Color [X11Color White]] (const []) n
                                , globalAttributes =
                                    [ GraphAttrs [Ordering "out"] -- child nodes are drawn in edge-order
                                    , NodeAttrs [Shape PlainText]
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
        return (i, j, x)

    subtrees t = t : concatMap subtrees (T.subForest t)


toPng :: AVLTree Int -> String
toPng tree = unsafePerformIO $ do
   let fname = (hex $ fromIntegral $ hash tree) ++ ".png"
   runGraphviz (toDot tree)
               Png
               (picsDir </> fname)
   return $ showHtml
          $ image ! [ src ("../pics/" ++ fname), alt (show $ bonnifyTree tree) ]
