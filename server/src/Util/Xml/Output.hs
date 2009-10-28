module Util.Xml.Output (
    outputToXmlString
) where

import qualified Util.Xml.OutputDTD as X
import qualified Autolib.Output as O
import Text.XML.HaXml
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ

import qualified Codec.Binary.Base64.String as C

outputToXOutput :: O.Output -> X.Output
outputToXOutput o = case o of
    O.Empty ->
        X.OBeside $ X.Beside []
    O.Doc doc ->
        outputToXOutput $ O.Pre (show doc)
    O.Text txt ->
        X.OText $ X.Text txt
    O.Pre txt ->
        X.OPre $ X.Pre (show txt)
    O.Image img ->
        X.OImage $
            X.Image (X.Image_Attrs { X.imageType = "png",
                                     X.imageAlt = "<image>" })
                    (base64 img)
    O.Link uri ->
        outputToXOutput (O.Named_Link uri uri)
    O.Named_Link txt uri ->
        X.OLink $ X.Link (X.Link_Attrs { X.linkHref = uri }) txt
    O.Above o1 o2 ->
        X.OAbove $ X.Above $ map outputToXOutput (aboves o1 ++ aboves o2)
    O.Beside o1 o2 ->
        X.OBeside $ X.Beside $ map outputToXOutput (besides o1 ++ besides o2)
    O.Itemize os ->
        X.OItemize $ X.Itemize $ map outputToXOutput os
    O.Nest o ->
        X.OBeside $ X.Beside [nestSpacing, outputToXOutput o]

wrapXOutput :: X.Output -> Document
wrapXOutput o = let [CElem e] = toElem o in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST e []

xmlToString :: Document -> String
xmlToString = renderStyle style . document where
    style = Style OneLineMode 0 0

outputToXmlString = xmlToString . wrapXOutput . outputToXOutput

nestSpacing = X.OSpace $ X.Space {
    X.spaceWidth = "4",
    X.spaceHeight = "0",
    X.spaceUnit = "em" }
besides (O.Beside a b) = besides a ++ besides b
besides a = [a]

aboves (O.Above a b) = aboves a ++ aboves b
aboves a = [a]

base64 = C.encode . map (toEnum . fromIntegral) . octets

octets = map (fromIntegral . fromEnum)
