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

get_this :: VNr -> IO [ Vorlesung ]
get_this vnr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "VNr", "Name" ] )
        [ From $ map reed [ "vorlesung" ] 
        , Where $ equals ( reed "vorlesung.VNr" ) ( toEx vnr )
        ]
    common stat

get_tutored :: SNr -> IO [ Vorlesung ]
get_tutored snr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "vorlesung.VNr AS VNr"
			    , "vorlesung.Name AS Name" ] )
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
        ( Select $ map reed [ "vorlesung.VNr AS VNr"
			    , "vorlesung.Name AS Name" ] )
        [ From $ map reed [ "stud_grp", "gruppe", "vorlesung" ] 
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


teilnehmer :: VNr -> IO [ MNr ]
teilnehmer vnr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "student.MNr AS MNr" ] )
        [ From $ map reed [ "student", "stud_grp", "gruppe" ] 
	, Where $ ands
	        [ equals ( toEx vnr )  ( reed "gruppe.VNr" )
		, equals ( reed "gruppe.GNr" ) ( reed "stud_grp.GNr" )
		, equals ( reed "stud_grp.SNr" ) ( reed "student.SNr" )
		]
	]
    collectRows ( \ state -> getFieldValue state "MNr" ) stat

