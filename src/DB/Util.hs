module DB.Util where

import DB.Data

import Text.XML.HaXml
import Text.XML.HaXml.Haskell2Xml

import IO

fromCon :: Haskell2Xml a => Contents -> a
fromCon cs = let (x, rest) = fromContents $ unContents cs
	     in  x

readUntilBlankLine :: Handle -> IO String
readUntilBlankLine h = do
    cs <- hGetLine h
    if not $ null cs
       then do css <- readUntilBlankLine h
	       return $ cs ++ "\n" ++ css
       else do return "\n"

hReadXmlUtil ::  Haskell2Xml a => Handle -> IO a
-- the parser doesn't know when to stop?
hReadXmlUtil h = do
    -- hSetBuffering h LineBuffering
    content <- readUntilBlankLine h
    -- hPutStrLn stderr $ "input:\n\n" ++ content
    return (fromXml (xmlParse "<unknown>" content))

hWriteXmlUtil :: Haskell2Xml a => Handle -> a -> IO ()
hWriteXmlUtil h x = do
    -- hSetBuffering h LineBuffering
    hWriteXml h x
    hPutStrLn h ""
    hFlush    h
