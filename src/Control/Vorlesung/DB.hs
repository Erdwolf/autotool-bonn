module Control.Vorlesung.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Vorlesung.Typ
import qualified Control.Student.Type as CST
import qualified Control.Student.DB

import Prelude hiding ( all )

qq =  Select 
     $ map reed [ "vorlesung.VNr as VNr" 
		, "vorlesung.UNr as UNr" 
		, "vorlesung.ENr as ENr" 
		, "vorlesung.Name as Name"
		, "vorlesung.EinschreibVon as Von"
		, "vorlesung.EinschreibBis as Bis"
	        , "NOW() < vorlesung.EinschreibVon as Early"
		, "NOW() > vorlesung.EinschreibBis as Late"
                , "vorlesung.motd as Motd"
		] 

-- | get alle vorlesungen aus DB
-- TODO: implementiere filter
get :: IO [ Vorlesung ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "vorlesung" ] ]
    res <- common stat
    disconnect conn
    return res

get_at_school :: UNr -> IO [ Vorlesung ]
get_at_school unr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "vorlesung" ] 
        , Where $ equals ( reed "vorlesung.UNr" ) ( toEx unr )
        ]
    res <- common stat
    disconnect conn
    return res

get_this :: VNr -> IO [ Vorlesung ]
get_this vnr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "vorlesung" ] 
        , Where $ equals ( reed "vorlesung.VNr" ) ( toEx vnr )
        ]
    res <- common stat
    disconnect conn
    return res

get_tutored :: SNr -> IO [ Vorlesung ]
get_tutored snr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "tutor", "vorlesung" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "tutor.SNr" ) 
		, equals ( reed "tutor.VNr" ) ( reed "vorlesung.VNr" )
		]
	]
    res <- common stat
    disconnect conn
    return res

get_attended :: SNr -> IO [ Vorlesung ]
get_attended snr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "stud_grp", "gruppe", "vorlesung" ] 
	, Where $ ands
	        [ equals ( toEx snr ) ( reed "stud_grp.SNr" ) 
		, equals ( reed "stud_grp.GNr" ) ( reed "gruppe.GNr" )
		, equals ( reed "gruppe.VNr" ) ( reed "vorlesung.VNr" )
		]
	]
    res <- common stat
    disconnect conn
    return res



common = collectRows $ \ state -> do
    	g_vnr <- getFieldValue state "VNr"
    	g_unr <- getFieldValue state "UNr"
    	g_enr <- getFieldValue state "ENr"
        g_name <- getFieldValue state "Name"
        g_von <- getFieldValue state "Von"
        g_bis <- getFieldValue state "Bis"
        -- NOTE: the SQL type of a boolean expression is still int (in 1.4)
	g_early  <- getFieldValue state "Early"
	g_late   <- getFieldValue state "Late"
        g_motd   <- getFieldValue state "Motd"
        return $ Vorlesung { vnr = g_vnr
			   , unr = g_unr
			   , enr = g_enr
			 , name = g_name
			  , einschreibVon = g_von
			  , einschreibBis = g_bis
			  , einschreib = timer g_early g_late 
                          , motd = g_motd
    			   }



snr_teilnehmer :: VNr -> IO [ SNr ]
snr_teilnehmer vnr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "stud_grp.SNr as SNr" 
			    ] 
	)
        [ From $ map reed [ "stud_grp", "gruppe" ] 
	, Where $ ands
	        [ equals ( toEx vnr )  ( reed "gruppe.VNr" )
		, equals ( reed "gruppe.GNr" ) ( reed "stud_grp.GNr" )
		]
	]
    res <- collectRows ( \ state -> do
        getFieldValue state "SNr" 
      ) stat
    disconnect conn
    return res
    

steilnehmer :: VNr -> IO [ CST.Student ]
steilnehmer vnr = do
    snrs <- snr_teilnehmer vnr
    items <- sequence $ do
        snr <- snrs
        return $ Control.Student.DB.get_snr snr
    return $ concat items

teilnehmer :: VNr -> IO [ ( SNr, (MNr, Name, Name) ) ]
teilnehmer vnr = do
    studs <- steilnehmer vnr
    return $ do
        stud <- studs
        return ( CST.snr stud
               , ( CST.mnr stud, CST.vorname stud, CST.name stud )
               )

-- | put into table:
-- do not evaluate Vorlesung.vnr (it may be undefined!)
-- instead use first argument: Just vnr -> update, Nothing -> insert
put :: Maybe VNr
    -> Vorlesung
    -> IO ()
put mvnr vor = do
    conn <- myconnect 
    let common = [ ( reed "VNr", toEx $ vnr vor )
		 , ( reed "Name", toEx $ name vor )
		 , ( reed "Motd", toEx $ motd vor )
		 , ( reed "EinschreibVon", toEx $ einschreibVon vor )
		 , ( reed "EinschreibBis", toEx $ einschreibBis vor )
		 ]
    case mvnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "vorlesung") common ) 
	    [ ]
         Just vnr -> squery conn $ Query
            ( Update (reed "vorlesung") common ) 
	    [ Where $ equals ( reed "vorlesung.VNr" ) ( toEx vnr ) ]
    disconnect conn

-- | delete
delete :: VNr
    -> IO ()
delete vnr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "vorlesung" ) )
	[ Where $ equals ( reed "vorlesung.VNr" ) ( toEx vnr ) ]
    disconnect conn

