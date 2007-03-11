module Control.Tutor.DB where

import Control.SQL
import Control.Types
import qualified Control.Student
import qualified Control.Vorlesung

import Prelude hiding ( all )

get_tutored :: Control.Student.Student 
	     -> IO [ Control.Vorlesung.Vorlesung ]
get_tutored s = Control.Vorlesung.get_from_where
         ( map reed [ "tutor", "vorlesung" ] )
	 $ ands
	      [ equals ( toEx $ Control.Student.snr s ) ( reed "tutor.SNr" ) 
	      , equals ( reed "vorlesung.VNr" ) ( reed "tutor.VNr" )
	      ]

get_tutors :: Control.Vorlesung.Vorlesung
	   -> IO [ Control.Student.Student ]
get_tutors v = Control.Student.get_from_where
      ( map reed [ "student", "tutor" ] )
      $ ands
            [ equals ( reed "student.SNr" ) ( reed "tutor.SNr" )
	    , equals ( reed "tutor.VNr" ) ( toEx $ Control.Vorlesung.vnr v )
	    ]

put :: Control.Student.Student -> Control.Vorlesung.Vorlesung -> IO ()
put s v = do
    conn <- myconnect
    squery conn $ Query 
                ( Insert  ( reed "tutor" ) 
                 [ ( reed "SNr", toEx $ Control.Student.snr s )
		 , ( reed "VNr", toEx $  Control.Vorlesung.vnr v )
		 ] ) []
    disconnect conn

-- | delete
delete :: Control.Student.Student -> Control.Vorlesung.Vorlesung -> IO ()
delete s v  = do
    conn <- myconnect 
    squery conn $ Query
        ( Delete $ reed "tutor" ) 
	[ Where $ ands
	      [ equals ( reed "tutor.SNr" ) ( toEx $ Control.Student.snr s ) 
	      , equals ( reed "tutor.VNr" ) ( toEx $ Control.Vorlesung.vnr v )
	      ]
        ]
    disconnect conn

