module Util.Cache where

-- $Id$

import Util.Datei

cache :: (Show a, Read a) => Datei -> IO a -> IO a
-- falls schon vorhanden, dann lesen
-- sonst erzeugen und schreiben
cache d action = 
    catch ( do putStr $ "lies cache-file " ++ show d ++ " ... "
	       cs <- lesen d
	       putStrLn $ "OK"
	       return $ read cs
	  ) 
	  ( \ _ -> do 
		putStr $ "nicht vorhanden, schreibe cache ... "
	        x <- action
		schreiben d $ show x
		putStrLn $ "OK"
		return x
	  )


