module Util.Cache where

-- $Id$

import Util.Datei

import Util.Debug

cache :: (Show a, Read a) => Datei -> IO a -> IO a
-- falls schon vorhanden, dann lesen
-- sonst erzeugen und schreiben
cache d action = 
    catch ( do debug $ "lies cache-file " ++ show d ++ " ... "
	       cs <- lesen d
	       debug $ "OK"
	       return $ read cs
	  ) 
	  ( \ _ -> do 
		debug $ "nicht vorhanden, schreibe cache ... "
	        x <- action
		schreiben d $ show x
		debug $ "OK"
		return x
	  )


