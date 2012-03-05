{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, FlexibleInstances #-}
module HeapSort.GraphViz (toPng) where

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

import Hex (hex)

picsDir = ".."</>"pics"

instance Hashable a => Hashable (T.Tree a) where
    hash = hash . T.levels

toDot :: T.Tree Int -> DotGraph Int
toDot = graphToDot params
  where
    params = nonClusteredParams { globalAttributes =
                                    [ GraphAttrs [Ordering "out"] -- child nodes are drawn in edge-order
                                    , NodeAttrs [Shape PlainText]
                                    ]
                                , isDirected = True
                                }

toPng :: T.Tree Int -> String
toPng tree = unsafePerformIO $ do
   let fname = (hex $ fromIntegral $ hash tree) ++ ".png"
   runGraphviz (toDot tree)
               Png
               (picsDir </> fname)
   return $ showHtml
          $ image ! [ src ("../pics/" ++ fname), alt (show $ tree) ]
