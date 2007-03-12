module Control.Semester.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Semester.Typ


import Prelude hiding ( all )

qq =  Select 
     $ map reed [ "semester.ENr as ENr" 
		, "semester.UNr as UNr" 
		, "semester.Name as Name"
		, "semester.Von as Von"
		, "semester.Bis as Bis"
	        , "NOW() < semester.Von as Early"
		, "NOW() > semester.Bis as Late"
		] 

-- | get alle Semester aus DB
-- TODO: implementiere filter
get :: IO [ Semester ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "semester" ] ]
    res <- common stat
    disconnect conn
    return res

get_at_school :: UNr -> IO [ Semester ]
get_at_school unr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "semester" ] 
        , Where $ equals ( reed "semester.UNr" ) ( toEx unr )
        ]
    res <- common stat
    disconnect conn
    return res

get_this :: ENr -> IO [ Semester ]
get_this enr = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "semester" ] 
        , Where $ equals ( reed "semester.ENr" ) ( toEx enr )
        ]
    res <- common stat
    disconnect conn
    return res



common = collectRows $ \ state -> do
    	g_enr <- getFieldValue state "ENr"
    	g_unr <- getFieldValue state "UNr"
        g_name <- getFieldValue state "Name"
        g_von <- getFieldValue state "Von"
        g_bis <- getFieldValue state "Bis"
        -- NOTE: the SQL type of a boolean expression is still int (in 1.4)
	g_early  <- getFieldValue state "Early"
	g_late   <- getFieldValue state "Late"
        return $ Semester { enr = g_enr
			   , unr = g_unr
			 , name = g_name
			  , von = g_von
			  , bis = g_bis
			  , status = timer g_early g_late 
    			   }

-- | put into table:
-- do not evaluate Semester.enr (it may be undefined!)
-- instead use first argument: Just enr -> update, Nothing -> insert
put :: Maybe ENr
    -> Semester
    -> IO ()
put menr sem = do
    conn <- myconnect 
    let common = [ ( reed "UNr", toEx $ unr sem )
		 , ( reed "Name", toEx $ name sem )
		 , ( reed "Von", toEx $ von sem )
		 , ( reed "Bis", toEx $ bis sem )
		 ]
    case menr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "semester") common ) 
	    [ ]
         Just enr -> squery conn $ Query
            ( Update (reed "semester") common ) 
	    [ Where $ equals ( reed "semester.ENr" ) ( toEx enr ) ]
    disconnect conn

-- | delete
delete :: ENr
    -> IO ()
delete enr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "semester" ) )
	[ Where $ equals ( reed "semester.ENr" ) ( toEx enr ) ]
    disconnect conn

