-- | alles aufsammeln, was mit Rechten zu tun hat

module Control.Admin.DB where


import Control.SQL
import Control.Types
import qualified Control.Student

import Prelude hiding ( all )

get_tutored :: SNr -> IO [ VNr ]
get_tutored snr = do
    conn <- myconnect
    stat <- squery conn $ Query 
            ( Select $ map reed [ "tutor.VNr as VNr" ] )
        [ From $ map reed [ "tutor" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "tutor.SNr" ) 
		]
	]
    res <- collectRows ( \ state -> getFieldValue state "VNr" ) stat
    disconnect conn
    return res



is_minister :: Control.Student.Student -> IO Bool
is_minister s = do
    ms <- get_ministered s
    return $ not $ null ms

get_ministered :: Control.Student.Student -> IO [()]
get_ministered s = do
    conn <- myconnect
    stat <- squery conn $ Query ( Select [ reed "minister.SNr" ] )
        [ From $ map reed [ "minister" ] 
	, Where $ ands
	        [ equals ( toEx $ Control.Student.snr s ) 
                         ( reed "minister.SNr" ) 
		]
	]
    res <- collectRows ( \ state -> return () ) stat
    disconnect conn
    return res

