module Control.Aufgabe.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Aufgabe.Typ

import Prelude hiding ( all )

-- | get alle aufgaben aus DB
-- TODO: implementiere filter
get :: IO [ Aufgabe ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "ANr", "VNr", "Name"
			    , "Typ", "Config", "Remark"
			    , "Highscore", "Von" , "Bis"
			    ]
	)
        [ From $ map reed [ "aufgabe" ] 
        -- , order_by
	]
    inh  <- collectRows (\ state -> do
        g_anr <- getFieldValue state "ANr"
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"
        g_typ <- getFieldValue state "Typ"
        g_config <- getFieldValue state "Config"
        g_remark <- getFieldValue state "Remark"
        g_highscore <- getFieldValue state "Highscore"
        g_von <- getFieldValue state "Von"
        g_bis <- getFieldValue state "Bis"

        return $ Aufgabe { anr = g_anr
    			   , vnr = g_vnr
			 , name = g_name
    			 , highscore = g_highscore
    			   , von = g_von
    			   , bis = g_bis
    			   , typ = g_typ
    			   , config = g_config
    			   , remark = g_remark
    			   }
                    ) stat
    return inh



