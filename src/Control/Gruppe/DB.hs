module Control.Gruppe.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Gruppe.Typ

import Prelude hiding ( all )

qq =  Select 
     $ map reed [ "gruppe.GNr as GNr" 
		, "gruppe.VNr as VNr" 
		, "gruppe.Name as Name"
		, "gruppe.MaxStudents as MaxStudents"
		, "gruppe.Referent as Referent"
		] 

-- | get alle gruppen aus DB
-- TODO: implementiere filter
get :: IO [ Gruppe ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "gruppe" ] ]
    res <- common stat
    disconnect conn
    return res

get_this :: VNr -> IO [ Gruppe ]
get_this vnr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "gruppe" ] 
        , Where $ equals ( reed "gruppe.VNr" ) ( toEx vnr )
        ]
    res <- common stat
    disconnect conn
    return res

common = collectRows $ \ state -> do
    	g_gnr <- getFieldValue state "GNr"
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"
	g_maxStudents <- getFieldValue state "MaxStudents"
	g_referent <- getFieldValue state "Referent"
        return $ Gruppe { gnr = g_gnr
			, vnr = g_vnr
			, name = g_name
			, maxStudents = g_maxStudents
			, referent = g_referent
    			}

teilnehmer :: GNr -> IO [ ( SNr, (MNr, Name, Name) ) ]
teilnehmer gnr = do
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
	        [ equals ( toEx gnr )  ( reed "gruppe.GNr" )
		, equals ( reed "gruppe.GNr" ) ( reed "stud_grp.GNr" )
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
-- do not evaluate Gruppe.gnr (it may be undefined!)
-- instead use first argument: Just gnr -> update, Nothing -> insert
put :: Maybe GNr
    -> Gruppe
    -> IO ()
put mgnr gruppe = do
    conn <- myconnect 
    let common = [ ( reed "VNr", toEx $ vnr gruppe )
		 , ( reed "Name", toEx $ name gruppe )
		 , ( reed "Referent", toEx $ referent gruppe )
		 , ( reed "MaxStudents", toEx $ maxStudents gruppe )
		 ]
    case mgnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "gruppe") common ) 
	    [ ]
         Just gnr -> squery conn $ Query
            ( Update (reed "gruppe") common ) 
	    [ Where $ equals ( reed "gruppe.GNr" ) ( toEx gnr ) ]
    disconnect conn

-- | delete
delete :: GNr
    -> IO ()
delete gnr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "gruppe" ) )
	[ Where $ equals ( reed "gruppe.GNr" ) ( toEx gnr ) ]
    disconnect conn


