module Control.Vorlesung.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Vorlesung.Typ

import Prelude hiding ( all )

-- | get alle vorlesungen aus DB
-- TODO: implementiere filter
get :: IO [ Vorlesung ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "VNr", "Name" ] )
        [ From $ map reed [ "vorlesung" ] ]
    common stat

get_tutored :: SNr -> IO [ Vorlesung ]
get_tutored snr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "vorlesung.VNr as VNr"
			    , "Vorlesung.Name as Name" ] )
        [ From $ map reed [ "tutor", "vorlesung" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "tutor.SNr" ) 
		, equals ( reed "tutor.VNr" ) ( reed "vorlesung.VNr" )
		]
	]
    common stat

get_attended :: SNr -> IO [ Vorlesung ]
get_attended snr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "vorlesung.VNr as VNr"
			    , "Vorlesung.Name as Name" ] )
        [ From $ map reed [ "stud_grp", "grupp", "vorlesung" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "stud_grp.SNr" ) 
		, equals ( reed "stud_grp.GNr" ) ( reed "gruppe.GNr" )
		, equals ( reed "gruppe.VNr" ) ( reed "vorlesung.VNr" )
		]
	]
    common stat



common = collectRows $ \ state -> do
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"

        return $ Vorlesung { vnr = g_vnr
			 , name = g_name
    			   }

