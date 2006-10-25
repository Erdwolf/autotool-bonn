module Util.Cache where

--  $Id$

import Util.Datei
import Debug

import Autolib.Reader
import Autolib.ToDoc

import Control.Monad ( when )
import qualified Control.Exception 

-- | falls schon vorhanden, dann lesen
-- sonst erzeugen und schreiben
cache :: ( ToDoc a, Reader a ) 
      => Datei -> IO a -> IO a
cache d action = Control.Exception.catch
          ( do debug $ "lies cache-file " ++ show d ++ " ... "
	       input <- lesen d
	       debug $ "OK"

	       case parse ( parsec_wrapper 0 ) ( show d ) input of 
	           Right (x, "") -> 
		       return x
		   Left  err       -> do
	               let msg = input ++ "\n" ++ show err
		       debug msg
		       error msg
	  ) 
	  ( \ ex -> do 
		debug $ "Exception " ++ show ex ++ ", schreibe cache neu ... "
	        x <- action
		schreiben d $ show $ toDoc x
		debug $ "OK"
		return x
	  )


