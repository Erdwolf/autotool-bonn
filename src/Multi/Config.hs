module Multi.Config where

-- $Id$

import Util.Datei


data Config = 
     Config { prefix :: Datei -- nur dir-name
	    , objects :: Int
	    , names :: Int
	    , tries :: Int
	    , tries_left :: Int
	    }

