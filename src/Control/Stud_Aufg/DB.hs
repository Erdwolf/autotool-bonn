module Control.Stud_Aufg.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Stud_Aufg.Typ

import Prelude hiding ( all )

-- | alle einsendungen zu dieser aufgabe
get_anr :: ANr -> IO [ Stud_Aufg ]
get_anr anr = 
    get_where [ equals ( reed "stud_aufg.ANr" ) ( toEx anr ) ]

-- | alle einsendungen dieser aufgabe, dieses studenten
get_snr_anr :: SNr -> ANr -> IO [ Stud_Aufg ]
get_snr_anr snr anr = 
    get_where [ equals ( reed "stud_aufg.ANr" ) ( toEx anr ) 
	      , equals ( reed "stud_aufg.SNr" ) ( toEx snr ) 
	      ]

get_where :: [ Expression ] -> IO [ Stud_Aufg ]
get_where wh = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "SNr", "ANr", "Ok", "No"
			    , "Instant", "Result", "Input", "Report" 
			    ] )
        [ From $ map reed [ "stud_aufg" ] 
        , Where $ ands wh
	]
    inh  <- collectRows (\ state -> do
        s_snr <- getFieldValue state "SNr"
    	s_anr <- getFieldValue state "ANr"
        s_ok <- getFieldValue state "Ok"
        s_no <- getFieldValue state "No"
        s_instant <- getFieldValueMB state "Instant"
        s_result <- getFieldValueMB state "Result"
        s_input <- getFieldValueMB state "Input"
        s_report <- getFieldValueMB state "Report"
        return $ Stud_Aufg { snr = s_snr
    			   , anr = s_anr
			 , ok = s_ok
			 , no = s_no
			   , instant = s_instant
			   , result = s_result
			   , input = s_input
			   , report = s_report
    			   }
                    ) stat
    disconnect conn
    return inh
    




