module Control.Aufgabe where

--  $Id$

import Control.Types
import Control.Aufgabe.Typ

getHighscoreAufgabeTypesDB :: IO [ Aufgabe ]
getHighscoreAufgabeTypesDB =
    do
    { let { sqlstr = unwords
              [ "SELECT "
              , " CONCAT(aufgabe.Name,\"-\",aufgabe.Subject ) as aufg"
              , ", Highscore"
	      , ", VNr"
	      , ", Von"
	      , ", Bis"
	      , ", Type"
	      , ", Config"
              , "FROM aufgabe"
              , "ORDER BY aufg"
              , ";" 
              ]  
        }
    ; conn <- myconnect
    ; stat <- query conn $ sqlstr
    ; inh  <- collectRows (\ state ->
                         do
                                g_aufgabe <- getFieldValue state "aufg"
                                g_highscore <- getFieldValue state "Highscore"
				g_vnr <- getFieldValue state "VNr"
                                g_von <- getFieldValue state "Von"
                                g_bis <- getFieldValue state "Bis"
                                g_type <- getFieldValue state "Type"
                                g_config <- getFieldValue state "Config"

                                return $ Aufgabe { aufgabe = g_aufgabe
						   , direction = g_highscore
						   , vorlesung = g_vnr
						   , von = g_von
						   , bis = g_bis
						   , typ = g_type
						   , config = g_config
						   }
                        ) stat
    ; return inh
    }


