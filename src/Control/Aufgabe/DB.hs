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

-- | put into table:
-- do not evaluate Aufgabe.anr (it may be undefined!)
-- instead use first argument: Just anr -> update, Nothing -> insert
put :: Maybe ANr 
    -> Aufgabe
    -> IO ()
put manr auf = do
    conn <- myconnect 
    let common = [ ( reed "VNr", toEx $ vnr auf )
		 , ( reed "Name", toEx $ name auf )
		 , ( reed "Typ", toEx $ typ auf )
		 , ( reed "Config", toEx $ config auf )
		 , ( reed "Remark", toEx $ remark auf )
		 , ( reed "Highscore", toEx $ highscore auf )
		 , ( reed "Von", toEx $ von auf )
		 , ( reed "Bis", toEx $ bis auf )
		 ]
    stat <- case manr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "aufgabe") common ) 
	    [ ]
         Just anr -> squery conn $ Query
            ( Update (reed "aufgabe") common ) 
	    [ Where $ equals ( reed "aufgabe.ANr" ) ( toEx anr ) ]
    disconnect conn
