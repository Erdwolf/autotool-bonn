module Util.Cache where

--  $Id$

import Util.Datei
import Util.Debug

import Control.Monad ( when )
import qualified Control.Exception 

-- | falls schon vorhanden, dann lesen
-- sonst erzeugen und schreiben
cache :: (Show a, Read a) => Datei -> IO a -> IO a
cache d action = Control.Exception.catch
          ( do debug $ "lies cache-file " ++ show d ++ " ... "
	       cs <- lesen d
	       debug $ "OK"
               let a = read cs
	       -- the following forces evaluation of a ?
	       when ( 0 == length ( show a )) $ do
	            let msg = "cannot parse cache contents"
	            debug msg ; fail msg
	       return a 
	  ) 
	  ( \ _ -> do 
		debug $ "nicht vorhanden, schreibe cache ... "
	        x <- action
		schreiben d $ show x
		debug $ "OK"
		return x
	  )


