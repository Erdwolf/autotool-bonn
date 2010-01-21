module Control.Stud_Grp.DB where

--  $Id$

import Control.SQL
import Control.Types hiding ( ok )
import Control.Stud_Grp.Typ

import qualified Control.Exception as CE

import Prelude hiding ( all )

-- | einschreiben
insert :: SNr -> GNr -> IO ( )
insert snr gnr = do
    conn <- myconnect 
    try $ squery conn $ Query
	                  ( Insert (reed "stud_grp") 
			        [ ( reed "SNr", toEx $ snr )
				, ( reed "GNr", toEx $ gnr )
				]
			  )
			  [ ]
    disconnect conn

try :: IO a -> IO (Either CE.SomeException a)
try = CE.try

-- | anzahl der eingeschriebenen leute
attendance :: GNr -> IO Integer
attendance gnr = do
    conn <- myconnect
    state <- squery conn $ Query
        ( Select [ reed "count(SNr) as C" ] )
	[ From $ map reed [ "stud_grp " ] 
	, Where $ equals ( reed "stud_grp.GNr" ) ( toEx gnr )
	]
    cs <- collectRows ( \ state -> do
        getFieldValue state "C" ) state
    disconnect conn
    return $ sum cs

-- | ausschreiben
delete :: SNr -> GNr -> IO ()
delete snr gnr =  do
    conn <- myconnect 
    try $ squery conn $ Query
	                  ( Delete (reed "stud_grp") )
			  [ common snr gnr ]
    disconnect conn

common snr gnr = Where $ ands
	       [ equals ( reed "stud_grp.SNr" ) ( toEx snr ) 
	       , equals ( reed "stud_grp.GNr" ) ( toEx gnr ) 
	       ]

    




