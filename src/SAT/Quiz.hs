module SAT.Quiz where

-- $Id$

import SAT.Types
import SAT.Param
import SAT.Generator

import Inter.Types
import Reporter
import ToDoc

quiz :: String -- Aufgabe
     -> String -- Version
     -> Param
     -> Var SAT Formel Belegung
quiz auf ver par =  
         Var { problem = SAT
	     , aufgabe = auf
	     , version = ver
	     -- erzeugt cached version der instanz (o. �.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tats�chliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ key -> do
	           seed $ read key
	           ( p, f ) <- cache (  Datei { pfad = [ "autotool", "cache"
					   , auf, ver
					   ]
				  , name = key ++ ".cache" 
				  }
			 ) ( generator par )
	           return $ do
	               inform $ vcat
	                  [ text "Finden Sie eine erf�llende Belegung f�r diese Formel:
			  , nest 4 $ toDoc p
			  ]
	               return p
	     }


var = Var    {  problem = SAT
	     , variant = "simple"
	     , key = \ matrikel -> do
	         -- d. h. jeder bekommt immer die gleiche aufgabe
	         return matrikel
	     , gen = \ matrikel -> do
                 let f = bsp_formel
	         inform $ text "Finden Sie eine erf�llende Belegung f�r"
	         inform $ toDoc f
	         return f
	     }

