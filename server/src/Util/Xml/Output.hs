-- Utility functions for dealing with the conversion of Output to Xml

module Util.Xml.Output (
    outputToXmlString,
    stringToXmlString,
    xmlStringToOutput
) where

import qualified Util.Xml.OutputDTD as X
import qualified Autolib.Output as O
import Text.XML.HaXml hiding (o, txt)
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.XmlContent
import Text.PrettyPrint.HughesPJ hiding (style)
import qualified Autolib.Multilingual.Doc as D

import qualified Codec.Binary.Base64 as C
import qualified Data.ByteString as B
import System.FilePath
import Control.Applicative
import Data.Maybe

import Util.Png

outputToXOutput :: O.Output -> IO X.Output
outputToXOutput o = case o of
    O.Empty ->
        return $ X.OBeside $ X.Beside []
    O.Doc doc ->
        outputToXOutput $ O.Pre doc
    O.Text txt ->
        return $ X.OText $ X.Text txt
    O.Pre txt ->
        return $ X.OPre $ X.Pre (show txt)
    O.Image file contents -> do
        let ext = drop 1 $ snd $ splitExtension file
        contents' <- contents
        let (w, h) = case ext of
                "png" -> (pngSize contents')
                _     -> (0, 0)
            img = C.encode (B.unpack contents')
        return $ X.OImage $
            X.Image (X.Image_Attrs { X.imageType = ext,
                                     X.imageAlt = "<image>",
                                     X.imageUnit = "px",
                                     X.imageWidth = show w,
                                     X.imageHeight = show h })
                    img
    O.Link uri ->
        outputToXOutput (O.Named_Link uri uri)
    O.Named_Link txt uri ->
        return $ X.OLink $ X.Link (X.Link_Attrs { X.linkHref = uri }) txt
    O.Above o1 o2 ->
        X.OAbove . X.Above <$> mapM outputToXOutput (aboves o1 ++ aboves o2)
    O.Beside o1 o2 ->
        X.OBeside . X.Beside <$> mapM outputToXOutput (besides o1 ++ besides o2)
    O.Itemize os ->
        X.OItemize . X.Itemize <$> mapM outputToXOutput os
    O.Nest o' ->
        X.OBeside . X.Beside <$> sequence [return nestSpacing, outputToXOutput o']
    O.Figure a b ->
        X.OFigure <$> (X.Figure <$> outputToXOutput a <*> outputToXOutput b)

xoutputToOutput :: X.Output -> O.Output
xoutputToOutput o = case o of
   X.OPre  (X.Pre  txt) -> O.Pre (D.text txt)
   X.OText (X.Text txt) -> O.Text txt
   X.OImage (X.Image _ img) ->
       O.Image (mkData img) (return $ B.pack $ fromJust $ C.decode img)
   X.OLink (X.Link (X.Link_Attrs { X.linkHref = uri }) txt) ->
       O.Named_Link txt uri
   X.OAbove (X.Above []) -> O.Empty
   X.OAbove (X.Above xs) -> foldl1 O.Above $ map xoutputToOutput xs
   X.OBeside (X.Beside []) -> O.Empty
   X.OBeside (X.Beside xs) -> foldl1 O.Beside $ map xoutputToOutput xs
   X.OItemize (X.Itemize xs) -> O.Itemize $ map xoutputToOutput xs
   X.OSpace _ -> O.Empty -- FIXME
   X.OFigure (X.Figure a b) -> O.Figure (xoutputToOutput a) (xoutputToOutput b)


mkData = ("data:image/png;base64," ++)

wrapXOutput :: X.Output -> Document ()
wrapXOutput o = let [CElem e _] = toContents o in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST e []

xmlToString :: Document () -> String
xmlToString = renderStyle style . document where
    style = Style OneLineMode 0 0

outputToXmlString ::  O.Output -> IO String
outputToXmlString = fmap (xmlToString . wrapXOutput) . outputToXOutput

xmlStringToOutput :: String -> O.Output
xmlStringToOutput = xoutputToOutput . either error id . readXml

stringToXmlString :: String -> String
stringToXmlString = xmlToString . wrapXOutput . X.OText . X.Text

-- helpers for outputToXOutput
nestSpacing :: X.Output
nestSpacing = X.OSpace $ X.Space {
    X.spaceWidth = "4",
    X.spaceHeight = "0",
    X.spaceUnit = "em" }

besides :: O.Output -> [O.Output]
besides (O.Beside a b) = besides a ++ besides b
besides a = [a]

aboves :: O.Output -> [O.Output]
aboves (O.Above a b) = aboves a ++ aboves b
aboves a = [a]
