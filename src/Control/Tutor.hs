module Control.Tutor where

--  $Id$

import Control.Types
import Control.SQL

-- | find all vorlesungen that are tutored by this person
turored :: SNr -> IO [ VNr ]
turored snr = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "VNr" ] )
        [ From $ map reed [ "tutor" ] 
        , Where $ equals ( reed "tutor.SNr" ) ( toEx snr )
	]
    inh  <- collectRows (\ state -> do
        getFieldValue state "VNr"
                    ) stat
    return inh



