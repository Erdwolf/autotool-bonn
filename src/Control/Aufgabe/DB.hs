module Control.Aufgabe.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Aufgabe.Typ

import Data.Maybe
import Prelude hiding ( all )

-- | get alle aufgaben aus DB
-- TODO: implementiere filter
get_this :: ANr -> IO [ Aufgabe ]
get_this anr = select_where [ equals ( reed "aufgabe.ANr" ) ( toEx anr ) ]

get :: Maybe VNr 
    -> IO [ Aufgabe ]
get mvnr = select_where $
	        [ equals ( reed "aufgabe.VNr" ) ( toEx vnr ) 
		| vnr <- maybeToList mvnr
		] 

select_where wh = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "ANr", "VNr", "Name"
			    , "Typ", "Config", "Remark"
			    , "Highscore", "Status", "Von" , "Bis"
			    , "NOW() BETWEEN ( Von AND Bis ) as Current"
			    ]
	) $
        [ From $ map reed [ "aufgabe" ] 
        , Where $ ands wh
        ]
    res <- common stat
    disconnect conn
    return res

common = collectRows $ \ state -> do
        g_anr <- getFieldValue state "ANr"
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"
        g_typ <- getFieldValue state "Typ"
        g_config <- getFieldValue state "Config"
        g_remark <- getFieldValue state "Remark"
        g_highscore <- getFieldValue state "Highscore"
        g_status <- getFieldValue state "Status"
        g_von <- getFieldValue state "Von"
        g_bis <- getFieldValue state "Bis"
	g_current <- getFieldValue state "Current"

        return $ Aufgabe { anr = g_anr
    			   , vnr = g_vnr
			 , name = g_name
    			 , highscore = g_highscore
    			 , status = g_status
    			   , von = g_von
    			   , bis = g_bis
			  , current = g_current
    			   , typ = g_typ
    			   , config = g_config
    			   , remark = g_remark
    			   }


-- | put into table:
-- do not evaluate Aufgabe.anr (it may be undefined!)
-- instead use first argument: Just anr -> update, Nothing -> insert
put :: Maybe ANr 
    -> Aufgabe
    -> IO ()
put manr auf = do
    conn <- myconnect 
    let common = [ ( reed "VNr", toEx $ vnr auf )
		 , ( reed "Name", toEx $ name auf )
		 , ( reed "Typ", toEx $ typ auf )
		 , ( reed "Config", toEx $ config auf )
		 , ( reed "Remark", toEx $ remark auf )
		 , ( reed "Highscore", toEx $ highscore auf )
                 -- current wird nicht geputtet
		 , ( reed "Status", toEx $ status auf )
		 , ( reed "Von", toEx $ von auf )
		 , ( reed "Bis", toEx $ bis auf )
		 ]
    case manr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "aufgabe") common ) 
	    [ ]
         Just anr -> squery conn $ Query
            ( Update (reed "aufgabe") common ) 
	    [ Where $ equals ( reed "aufgabe.ANr" ) ( toEx anr ) ]
    disconnect conn

-- | delete
delete :: ANr 
    -> IO ()
delete anr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "aufgabe" ) )
	[ Where $ equals ( reed "aufgabe.ANr" ) ( toEx anr ) ]
    disconnect conn




