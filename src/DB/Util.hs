module DB.Util where

import Text.XML.HaXml
import Text.XML.HaXml.Haskell2Xml

import IO

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
    return (fromXml (xmlParse "<unknown>" content))

hWriteXmlUtil :: Haskell2Xml a => Handle -> a -> IO ()
hWriteXmlUtil h x = do
    -- hSetBuffering h LineBuffering
    hWriteXml h x
    hPutStrLn h ""
    hFlush    h
