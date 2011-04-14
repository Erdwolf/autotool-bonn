-- | wrapper for the system library

module Gateway.My_CGI2

( wrapper
)

--  $Id$

where

import Network.CGI hiding ( wrapper )
import Control.Monad.Trans

import Text.XHtml
import System.IO


-- | same type as official function
wrapper :: ([(String,String)] -> IO Html) -> IO ()
wrapper f = runCGI $ do
    e <- getInputs
    a <- lift $ f $ e
    output $ renderHtml a

