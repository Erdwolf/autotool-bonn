{-# LANGUAGE NoMonomorphismRestriction #-}
module Prolog.Programming.GraphViz (resolveWithTree, asInlinePng) where

import System.IO.Unsafe (unsafePerformIO)
import Text.XHtml (showHtml, Html, image, (!), src, alt, anchor, href)
import Data.GraphViz  --(runGraphviz, GraphvizCommand(Dot), setDirectedness, graphToDot, nonClusteredParams, fmtNode, fmtEdge, toLabel, GraphvizOutput(Png))
import Data.GraphViz.Attributes.HTML (HtmlTextItem)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types (GlobalAttributes(GraphAttrs))
import Language.Prolog (resolve_)
import Language.Prolog.GraphViz (runGraphGenT)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>),(<.>))
import System.IO (openTempFile, hClose)
import Control.Concurrent (threadDelay)
import Data.Hashable (Hashable(hash))

import Prolog.Programming.Hex (hex)

import Data.Graph.Inductive.Graph (labNodes, labEdges)
import Data.Graph.Inductive.Tree (Gr)

instance (Hashable a, Hashable b) => Hashable (Gr a b) where
   hash g = hash (labNodes g, labEdges g)

instance Hashable HtmlTextItem where
   hash = hash . show

resolveWithTree p q = runGraphGenT $ resolve_ p q

asInlinePng = showHtml . toPng


picsDir = ".."</>"pics"

toPng graph = unsafePerformIO $ do
   --tmp <- getTemporaryDirectory
   --(tmpfile, h) <- openTempFile tmp "graphviz"
   --hClose h
   let fname = (hex $ fromIntegral $ hash graph) ++ ".png"
   runGraphviz (setDirectedness graphToDot params graph)
               Png
               (picsDir </> fname)
   runGraphviz (setDirectedness graphToDot (params { globalAttributes = [GraphAttrs [Size $ createPoint 1 1]] }) graph)
               Png
               (picsDir </> "thumb_" ++ fname)
   return $ anchor ! [ href ("../pics/" ++ fname) ]
          $ image ! [ src ("../pics/thumb_" ++ fname), alt "Ableitungsbaum" ]
 where
   params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                               , fmtEdge = \(_, _, l) -> [toLabel l]
                               }

{-
inlinePng :: BS.ByteString -> Html
inlinePng pngImage =
     let
       -- Extract width and height from the PNG header
       header = BS.drop 16 pngImage
       w = valueOf $ BS.take 4 header
       h = valueOf $ BS.take 4 $ BS.drop 4 header
       valueOf = sum . zipWith (*) [256^3, 256^2, 256, 1]
                     . map fromEnum
                     . BS.unpack
     in
       image ! [ src $ "data:image/png;base64," ++ Base64.encode (BS.unpack pngImage)
               , alt "Herleitungsbaum"
               , width  (show w)
               , height (show h)
               ]
-}
