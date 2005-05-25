module Control.Student.DB where

--  $Id$

import Control.SQL
import Control.Types
import qualified Control.Student.Type as CST

import Prelude hiding ( all )

-- | get alle passenden studenten aus DB
-- TODO: implementiere filter
get_mnr  :: MNr -> IO [ CST.Student ]
get_mnr mnr = get_where $ equals ( reed "student.MNr" ) ( toEx mnr )

get_snr  :: SNr -> IO [ CST.Student ]
get_snr snr = get_where $ equals ( reed "student.SNr" ) ( toEx snr )

snr_by_mnr :: MNr -> IO [ SNr ]
snr_by_mnr mnr = get_mnr mnr >>= return . map CST.snr

get_where :: Expression -> IO [ CST.Student ]
get_where ex = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "SNr", "MNr", "Name", "Vorname" 
			    , "Email", "Passwort" 
			    ] )
        [ From $ map reed [ "student" ] 
        , Where $ ex
	]
    inh  <- collectRows (\ state -> do
        s_snr <- getFieldValue state "SNr"
    	s_mnr <- getFieldValue state "MNr"
        s_name <- getFieldValue state "Name"
        s_vorname <- getFieldValue state "Vorname"
        s_email <- getFieldValue state "Email"
        s_passwort <- getFieldValue state "Passwort"
        return $ CST.Student { CST.snr = s_snr
    			     , CST.mnr = s_mnr
			     , CST.name = s_name
			     , CST.vorname = s_vorname
			     , CST.email = s_email
			     , CST.passwort = s_passwort
    			     }
                    ) stat
    disconnect conn
    return inh


-- | put into table:
-- do not evaluate Student.snr (it may be undefined!)
-- instead use first argument: Just snr -> update, Nothing -> insert
put :: Maybe SNr 
    -> CST.Student
    -> IO ()
put msnr stud = do
    conn <- myconnect 
    let common = [ ( reed "Name", toEx $ CST.name stud )
		 , ( reed "Vorname", toEx $ CST.vorname stud )
		 , ( reed "Email", toEx $ CST.email stud )
		 , ( reed "Passwort", toEx $ CST.passwort stud )
		 ]
    -- stat <- case msnr of
    case msnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "student") 
	      $ ( reed "MNr" , toEx $ CST.mnr stud ) : common 
            ) 
	    [ ]
         Just snr -> squery conn $ Query
            ( Update (reed "student") common ) 
	    [ Where $ equals ( reed "student.SNr" ) ( toEx snr ) ]
    disconnect conn




