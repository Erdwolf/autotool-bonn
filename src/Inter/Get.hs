module Main where

--   $Id$

import Network.CGI
import Text.XHtml hiding ( text  )
import Control.Monad ( when )

import qualified Util.Datei as D
import qualified Exception

main :: IO ()
main = wrapper $ \ env -> 
     get env
	 `Exception.catch` \ err -> 
	      return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

get :: [(String, String)] -> IO Html
get env = 
    case lookup "file" env of
        Nothing -> return $ primHtml "value of file parameter not found"
        Just file -> do
	    let d = D.internalize file
	    when ( take 2 ( D.pfad d ) /= [ "autotool", "store" ] )
		 $ error "restricted area"
	    contents <- D.lesen d
	    return $ primHtml contents


