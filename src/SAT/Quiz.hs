module SAT.Quiz where

-- -- $Id$

import SAT.SAT
import SAT.Param
import SAT.Generator

import Inter.Types

import Reporter
import ToDoc

import Util.Datei
import Util.Cache
import Util.Seed


quiz :: String -- Aufgabe
     -> String -- Version
     -> Param
     -> Var SAT Formel Belegung
quiz auf ver par =  
         Var { problem = SAT
	     , aufgabe = auf
	     , version = ver
	     -- erzeugt cached version der instanz (o. ä.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tatsächliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ key -> do
	           seed $ read key
	           ( f, b ) <- cache (  Datei { pfad = [ "autotool", "cache"
					   , auf, ver
					   ]
				  , name = key
				  , extension = "cache" 
				  }
			 ) ( hgen2 par )
	           return $ return f
	     }


