module Control.Student.DB where

--  $Id$

import Control.SQL
import Control.Types
import Inter.Crypt
import qualified Control.Student.Type as CST

import Prelude hiding ( all )

-- | get alle passenden studenten aus DB
-- TODO: implementiere filter
get_unr_mnr  :: ( UNr , MNr ) -> IO [ CST.Student ]
get_unr_mnr ( unr , mnr ) = 
    get_where $ ands
	      [ equals ( reed "student.UNr" ) ( toEx unr )
	      , equals ( reed "student.MNr" ) ( toEx mnr )
	      ]

get_snr  :: SNr -> IO [ CST.Student ]
get_snr snr = get_where $ equals ( reed "student.SNr" ) ( toEx snr )

snr_by_unr_mnr :: ( UNr, MNr ) -> IO [ SNr ]
snr_by_unr_mnr um = get_unr_mnr um >>= return . map CST.snr

-- so findet man die Waisenkinder:
-- select student.* -- oder sogar *
-- from student left join stud_grp ON student.snr=stud_grp.snr 
-- where stud_grp.gnr is null;  

-- | alle, die in keiner Übungsgruppe sind
orphans :: UNr -> IO [ CST.Student ]
orphans unr = 
    let from = reed "student LEFT JOIN stud_grp ON student.snr = stud_grp.snr"
    in  get_from_where [ from ] $ ands
            [ equals ( reed "student.UNr" ) ( toEx unr )
	    , reed "stud_grp.gnr IS NULL"
	    ]

get_from_where from ex = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "student.SNr", "UNr", "MNr", "Name", "Vorname" 
			    , "Email", "Passwort", "Next_Passwort" 
			    ] )
        [ From $ from
        , Where $ ex
	]
    inh  <- collectRows (\ state -> do
        s_snr <- getFieldValue state "SNr"
    	s_unr <- getFieldValue state "UNr"
    	s_mnr <- getFieldValue state "MNr"
        s_name <- getFieldValue state "Name"
        s_vorname <- getFieldValue state "Vorname"
        s_email <- getFieldValue state "Email"
        s_passwort <- getFieldValue state "Passwort"
        s_next_passwort <- getFieldValue state "Next_Passwort"
        return $ CST.Student { CST.snr = s_snr
    			     , CST.unr = s_unr
    			     , CST.mnr = s_mnr
			     , CST.name = s_name
			     , CST.vorname = s_vorname
			     , CST.email = s_email
			     , CST.passwort = s_passwort
			     , CST.next_passwort = s_next_passwort
    			     }
                    ) stat
    disconnect conn
    return inh

get_where :: Expression -> IO [ CST.Student ]
get_where ex = get_from_where ( map reed [ "student" ] ) ex

-- | put into table:
-- do not evaluate Student.snr (it may be undefined!)
-- instead use first argument: Just snr -> update, Nothing -> insert
put :: Maybe SNr 
    -> CST.Student
    -> IO ()
put msnr stud = do
    conn <- myconnect 
    let common = [ ( reed "UNr", toEx $ CST.unr stud )
		 , ( reed "Name", toEx $ CST.name stud )
		 , ( reed "Vorname", toEx $ CST.vorname stud )
		 , ( reed "Email", toEx $ CST.email stud )
		 , ( reed "Passwort", toEx $ CST.passwort stud )
		 , ( reed "Next_Passwort", toEx $ CST.next_passwort stud )
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

-- not used
switch_passwort :: CST.Student -> IO ()
switch_passwort stud = do
    conn <- myconnect
    squery conn $ Query
           ( Update ( reed "student" )
                    [ ( reed "Passwort", reed "Next_Passwort" ) ] )
           [ Where $ equals ( reed "student.SNr" ) ( toEx $ CST.snr stud ) ]
    squery conn $ Query
           ( Update ( reed "student" )
                    [ ( reed "Passwort", toEx $ Crypt "" ) ] )
           [ Where $ equals ( reed "student.SNr" ) ( toEx $ CST.snr stud ) ]
    disconnect conn



