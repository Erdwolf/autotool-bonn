module Util.Xml.Output (
    outputToXmlString,
    stringToXmlString
) where

import qualified Util.Xml.OutputDTD as X
import qualified Autolib.Output as O
import Text.XML.HaXml hiding (o, txt)
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ hiding (style)

import qualified Codec.Binary.Base64 as C
import qualified Data.ByteString as B
import System.FilePath
import Control.Applicative

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
        img <- (C.encode . B.unpack) `fmap` contents
        return $ X.OImage $
            X.Image (X.Image_Attrs { X.imageType = ext,
                                     X.imageAlt = "<image>" })
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

wrapXOutput :: X.Output -> Document
wrapXOutput o = let [CElem e] = toElem o in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST e []

xmlToString :: Document -> String
xmlToString = renderStyle style . document where
    style = Style OneLineMode 0 0

outputToXmlString ::  O.Output -> IO String
outputToXmlString = fmap (xmlToString . wrapXOutput) . outputToXOutput

stringToXmlString :: String -> String
stringToXmlString = xmlToString . wrapXOutput . X.OText . X.Text

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
