{-# LANGUAGE NoMonomorphismRestriction #-}
module Prolog.Programming.GraphViz (resolveWithTree, asInlinePng) where

import qualified Data.ByteString as BS
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Binary.Base64 as Base64
import Text.XHtml (showHtml, Html, image, (!), src, alt, width, height)
import Data.GraphViz (graphvizWithHandle, GraphvizCommand(Dot), setDirectedness, graphToDot, nonClusteredParams, fmtNode, fmtEdge, toLabel, GraphvizOutput(Png))
import Language.Prolog (resolve_)
import Language.Prolog.GraphViz (runGraphGenT)


resolveWithTree p q = runGraphGenT $ resolve_ p q

asInlinePng = showHtml . inlinePng . toPng


toPng graph = unsafePerformIO $ do
   graphvizWithHandle Dot
                      (setDirectedness graphToDot params graph)
                      Png
                      BS.hGetContents
 where
   params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                               , fmtEdge = \(_, _, l) -> [toLabel l]
                               }

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

