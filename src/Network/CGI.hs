-- | replacement for the system library

module Network.CGI

( wrapper
)

--  $Id$

where

import FormData

import Text.Html
import System.IO


-- | same type as official function
wrapper :: ([(String,String)] -> IO Html) -> IO ()
wrapper f = do
    fd <- getFormData
    a <- f $ mkEnv fd
    putStr "Content-type: text/html\n\n"
    putStr (renderHtml a)

mkEnv :: FormData -> [(String, String)]
mkEnv fd = do
    ff <- fd
    return $ case ff of
        StandardField name      value -> ( name, value )
	FileField     name path value -> ( name, value )
