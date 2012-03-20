{-# LANGUAGE NoMonomorphismRestriction #-}
module Prolog.Programming.GraphViz (resolveWithTree, asInlinePng) where

import System.IO.Unsafe (unsafePerformIO)
import Text.XHtml (showHtml, Html, image, (!), src, alt, anchor, href)
import Data.GraphViz  --(runGraphviz, GraphvizCommand(Dot), setDirectedness, graphToDot, nonClusteredParams, fmtNode, fmtEdge, toLabel, GraphvizOutput(Png))
import Data.GraphViz.Attributes.HTML (HtmlTextItem)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types (GlobalAttributes(GraphAttrs))
import Language.Prolog (resolve_)
import Language.Prolog.GraphViz (runGraphGenT, Gr(..), toDot)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>),(<.>))
import System.IO (openTempFile, hClose)
import Control.Concurrent (threadDelay)
import Data.Hashable (Hashable(hash))

import Hex (hex)

--import Data.Graph.Inductive.Graph (labNodes, labEdges)

instance (Hashable a, Hashable b) => Hashable (Gr a b) where
   hash (Gr ns es) = hash (ns, es)

instance Hashable (DotGraph a) where
   hash = hash . show

resolveWithTree p q = runGraphGenT $ resolve_ p q

asInlinePng = showHtml . toPng


picsDir = ".."</>"pics"

toPng graph = unsafePerformIO $ do
   let fname = (hex $ fromIntegral $ hash $ toDot [] graph) ++ ".png"
   runGraphviz (toDot [] graph)
               Png
               (picsDir </> fname)
   runGraphviz (toDot [Size (createPoint 1 1)] graph)
               Png
               (picsDir </> "thumb_" ++ fname)
   return $ anchor ! [ href ("../pics/" ++ fname) ]
          $ image ! [ src ("../pics/thumb_" ++ fname), alt "Ableitungsbaum" ]
