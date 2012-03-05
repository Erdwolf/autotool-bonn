{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
module HeapSort.GraphViz (toPng) where

import HeapSort.Tree
import HeapSort.Semantics (Marked(..), undecorate)

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
import qualified Data.Tree
import Data.Traversable (traverse)
import Control.Monad.State (evalState, get, put)
import Tree.Class (ToTree(..))

import Hex (hex)

picsDir = ".."</>"pics"

instance ToTree (Data.Tree.Tree String) where
  toTree = id

instance ToTree (Tree String) where
  toTree = Data.Tree.unfoldTree uf
    where
      uf (Branch x Empty Empty) = (x,[])
      uf (Branch x l     Empty) = (x,[l])
      uf (Branch x Empty r    ) = (x,[r])
      uf (Branch x l     r    ) = (x,[l,r])

instance ToTree (Tree Int) where
  toTree = toTree . fmap show

instance ToTree (Tree (Marked Int)) where
  toTree = toTree . fmap showMarked
    where
      showMarked (Marked   x) = "[" ++ show x ++ "]"
      showMarked (Unmarked x) = show x

instance Hashable a => Hashable (Data.Tree.Tree a) where
    hash = hash . Data.Tree.levels

toDot :: ToTree (Tree a) => Tree a -> DotGraph Int
toDot t = graphElemsToDot params nodes edges
  where
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , globalAttributes =
                                    [ GraphAttrs [Ordering "out"] -- child nodes are drawn in edge-order
                                    , NodeAttrs [Shape PlainText]
                                    ]
                                , isDirected = True
                                }
    numbered =
      flip evalState 0 $ flip traverse (toTree t) $ \x -> do
        i <- get; put (succ i)
        return (i, x)

    nodes =
      flip map (Data.Tree.flatten numbered) $ \(i, x) -> (i, x)

    edges = do
        src@(Data.Tree.Node (i, _) _) <- subtrees numbered
        dst@(Data.Tree.Node (j, x) _) <- Data.Tree.subForest src
        return (i, j, x)

    subtrees t = t : concatMap subtrees (Data.Tree.subForest t)

class Numbers a where
    numbers :: Tree a -> [Int]

instance Numbers Int where
    numbers = toList
instance Numbers (Marked Int) where
    numbers = toList . undecorate


toPng :: Tree Int -> String
toPng tree = unsafePerformIO $ do
   let fname = (hex $ fromIntegral $ hash $ toTree tree) ++ ".png"
   runGraphviz (toDot tree)
               Png
               (picsDir </> fname)
   return $ showHtml
          $ image ! [ src ("../pics/" ++ fname), alt (show $ numbers tree) ]
