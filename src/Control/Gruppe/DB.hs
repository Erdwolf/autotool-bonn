module Control.Gruppe.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Gruppe.Typ

import Prelude hiding ( all )

qq =  Select 
     $ map reed [ "gruppe.VNr as VNr" 
		, "gruppe.Name as Name"
		, "gruppe.EinschreibVon as Von"
		, "gruppe.EinschreibBis as Bis"
	        , "NOW() < gruppe.EinschreibVon as Early"
		, "NOW() > gruppe.EinschreibBis as Late"
		] 

-- | get alle gruppeen aus DB
-- TODO: implementiere filter
get :: IO [ Gruppe ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "gruppe" ] ]
    common stat

get_this :: VNr -> IO [ Gruppe ]
get_this vnr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "gruppe" ] 
        , Where $ equals ( reed "gruppe.VNr" ) ( toEx vnr )
        ]
    common stat

get_tutored :: SNr -> IO [ Gruppe ]
get_tutored snr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "tutor", "gruppe" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "tutor.SNr" ) 
		, equals ( reed "tutor.VNr" ) ( reed "gruppe.VNr" )
		]
	]
    common stat

get_attended :: SNr -> IO [ Gruppe ]
get_attended snr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "stud_grp", "gruppe", "gruppe" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "stud_grp.SNr" ) 
		, equals ( reed "stud_grp.GNr" ) ( reed "gruppe.GNr" )
		, equals ( reed "gruppe.VNr" ) ( reed "gruppe.VNr" )
		]
	]
    common stat



common = collectRows $ \ state -> do
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"
        g_von <- getFieldValue state "Von"
        g_bis <- getFieldValue state "Bis"
	g_early <- getFieldValue state "Early"
	g_late <- getFieldValue state "Late"
        return $ Gruppe { vnr = g_vnr
			 , name = g_name
			  , einschreibVon = g_von
			  , einschreibBis = g_bis
			  , einschreib = timer g_early g_late
    			   }


teilnehmer :: VNr -> IO [ ( SNr, (MNr, Name, Name) ) ]
teilnehmer vnr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "student.SNr as SNr" 
			    , "student.MNr as MNr" 
			    , "student.Vorname as Vorname" 
			    , "student.Name as Name" 
			    ] 
	)
        [ From $ map reed [ "student", "stud_grp", "gruppe" ] 
	, Where $ ands
	        [ equals ( toEx vnr )  ( reed "gruppe.VNr" )
		, equals ( reed "gruppe.GNr" ) ( reed "stud_grp.GNr" )
		, equals ( reed "stud_grp.SNr" ) ( reed "student.SNr" )
		]
	]
    res <- collectRows ( \ state -> do
        s <- getFieldValue state "SNr" 
        m <- getFieldValue state "MNr" 
	v <- getFieldValue state "Vorname"
	n <- getFieldValue state "Name"
	return ( s, (m, v, n ))
      ) stat
    disconnect conn
    return res

-- | put into table:
-- do not evaluate Gruppe.vnr (it may be undefined!)
-- instead use first argument: Just vnr -> update, Nothing -> insert
put :: Maybe VNr
    -> Gruppe
    -> IO ()
put mvnr vor = do
    conn <- myconnect 
    let common = [ ( reed "VNr", toEx $ vnr vor )
		 , ( reed "Name", toEx $ name vor )
		 , ( reed "EinschreibVon", toEx $ einschreibVon vor )
		 , ( reed "EinschreibBis", toEx $ einschreibBis vor )
		 ]
    case mvnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "gruppe") common ) 
	    [ ]
         Just vnr -> squery conn $ Query
            ( Update (reed "gruppe") common ) 
	    [ Where $ equals ( reed "gruppe.VNr" ) ( toEx vnr ) ]
    disconnect conn

-- | delete
delete :: VNr
    -> IO ()
delete vnr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "gruppe" ) )
	[ Where $ equals ( reed "gruppe.VNr" ) ( toEx vnr ) ]
    disconnect conn

