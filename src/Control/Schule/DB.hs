module Control.Schule.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Schule.Typ
import qualified Control.Student.Type as CST
import qualified Control.Student.DB

import Prelude hiding ( all )

-- | get alle Schulen aus DB
-- TODO: implementiere filter
get :: IO [ Schule ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "schule" ] ]
    res <- common stat
    disconnect conn
    return res

gets :: String -> IO [ Schule ]
gets n = do
    conn <- myconnect
    stat <- squery conn $ Query qq
        [ From $ map reed [ "schule" ] 
        , Where $ equals ( reed "Name" ) ( EString n )
        ]
    res <- common stat
    disconnect conn
    return res


qq =  Select 
     $ map reed [ "schule.UNr as UNr" 
		, "schule.Name as Name"
		] 

common = collectRows $ \ state -> do
    	g_unr <- getFieldValue state "UNr"
        g_name <- getFieldValue state "Name"
        return $ Schule { unr = g_unr
			 , name = g_name
    			   }

-- | put into table:
put :: Maybe UNr
    -> Schule
    -> IO ()
put munr vor = do
    conn <- myconnect 
    let common = [ ( reed "UNr", toEx $ unr vor )
		 , ( reed "Name", toEx $ name vor )
		 ]
    case munr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "schule") common ) 
	    [ ]
         Just unr -> squery conn $ Query
            ( Update (reed "schule") common ) 
	    [ Where $ equals ( reed "schule.UNr" ) ( toEx unr ) ]
    disconnect conn

-- | delete
delete :: UNr
    -> IO ()
delete unr = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete ( reed "schule" ) )
	[ Where $ equals ( reed "schule.UNr" ) ( toEx unr ) ]
    disconnect conn

