module Control.Student.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Student.Type

import Prelude hiding ( all )

-- | get alle passenden studenten aus DB
-- TODO: implementiere filter
get :: MNr -> IO [ Student ]
get mnr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "SNr", "MNr", "Name", "Vorname" 
			    , "Email", "Passwort" 
			    ] )
        [ From $ map reed [ "student" ] 
        , Where $ equals ( reed "student.MNr" ) ( toEx mnr )
	]
    inh  <- collectRows (\ state -> do
        s_snr <- getFieldValue state "SNr"
    	s_mnr <- getFieldValue state "MNr"
        s_name <- getFieldValue state "Name"
        s_vorname <- getFieldValue state "Vorname"
        s_email <- getFieldValue state "Email"
        s_passwort <- getFieldValue state "Passwort"
        return $ Student { snr = s_snr
    			   , mnr = s_mnr
			 , name = s_name
			 , vorname = s_vorname
			   , email = s_email
			   , passwort = s_passwort
    			   }
                    ) stat
    return inh


-- | put into table:
-- do not evaluate Student.snr (it may be undefined!)
-- instead use first argument: Just snr -> update, Nothing -> insert
put :: Maybe SNr 
    -> Student
    -> IO ()
put msnr stud = do
    conn <- myconnect 
    let common = [ ( reed "Name", toEx $ name stud )
		 , ( reed "Vorname", toEx $ vorname stud )
		 , ( reed "Email", toEx $ email stud )
		 , ( reed "Passwort", toEx $ passwort stud )
		 ]
    stat <- case msnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "student") 
	      $ ( reed "MNr" , toEx $ mnr stud ) : common 
            ) 
	    [ ]
         Just snr -> squery conn $ Query
            ( Update (reed "student") common ) 
	    [ Where $ equals ( reed "student.SNr" ) ( toEx snr ) ]
    disconnect conn




