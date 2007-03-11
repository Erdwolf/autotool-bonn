module Control.Direktor.DB where

import Control.SQL
import Control.Types
import qualified Control.Student
import qualified Control.Schule

import Prelude hiding ( all )

get_directed :: Control.Student.Student 
	     -> IO [ Control.Schule.Schule ]
get_directed s = Control.Schule.get_from_where
         ( map reed [ "direktor", "schule" ] )
	 $ ands
	      [ equals ( toEx $ Control.Student.snr s ) ( reed "direktor.SNr" ) 
	      , equals ( reed "schule.UNr" ) ( reed "direktor.UNr" )
	      ]

get_directors :: Control.Schule.Schule -> IO [ Control.Student.Student ]
get_directors u = Control.Student.get_from_where
      ( map reed [ "student", "direktor" ] )
      $ ands
            [ equals ( reed "student.SNr" ) ( reed "direktor.SNr" )
	    , equals ( reed "direktor.UNr" ) ( toEx $ Control.Schule.unr u )
	    ]

put :: Control.Student.Student -> Control.Schule.Schule -> IO ()
put s u = do
    conn <- myconnect
    squery conn $ Query 
                ( Insert  ( reed "direktor" ) 
                 [ ( reed "SNr", toEx $ Control.Student.snr s )
		 , ( reed "UNr", toEx $  Control.Schule.unr u )
		 ] ) []
    disconnect conn

-- | delete
delete :: Control.Student.Student -> Control.Schule.Schule -> IO ()
delete s u  = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete $ reed "direktor" ) 
	[ Where $ ands
	      [ equals ( reed "direktor.SNr" ) ( toEx $ Control.Student.snr s ) 
	      , equals ( reed "direktor.UNr" ) ( toEx $  Control.Schule.unr u ) 
	      ]
        ]
    disconnect conn

