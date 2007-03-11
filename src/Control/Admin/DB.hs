-- | alles aufsammeln, was mit Rechten zu tun hat

module Control.Admin where


import Control.SQL
import Control.Types
import qualified Control.Student.DB

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

get_directed :: SNr -> IO [ UNr ]
get_directed snr = do
    conn <- myconnect
    stat <- squery conn $ Query 
            ( Select $ map reed [ "direktor.UNr as UNr" ] )
        [ From $ map reed [ "direktor" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "direktor.SNr" ) 
		]
	]
    res <- collectRows ( \ state -> getFieldValue state "UNr" ) stat
    disconnect conn
    return res

is_minister :: SNr -> IO Bool
is_minister snr = do
    ms <- get_ministered snr
    return $ not $ null ms

get_ministered :: SNr -> IO [()]
get_ministered snr = do
    conn <- myconnect
    stat <- squery conn $ Query ( Select [] )
        [ From $ map reed [ "minister" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "minister.SNr" ) 
		]
	]
    res <- collectRows ( \ state -> return () ) stat
    disconnect conn
    return res

