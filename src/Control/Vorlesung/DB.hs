module Control.Vorlesung.DB where

--  $Id$

import Control.SQL
import Control.Types
import Control.Vorlesung.Typ

import Prelude hiding ( all )

-- | get alle vorlesungen aus DB
-- TODO: implementiere filter
get :: IO [ Vorlesung ]
get = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "VNr", "Name"
			    ]
	)
        [ From $ map reed [ "vorlesung" ] 
        -- , order_by
	]
    inh  <- collectRows (\ state -> do
    	g_vnr <- getFieldValue state "VNr"
        g_name <- getFieldValue state "Name"

        return $ Vorlesung { vnr = g_vnr
			 , name = g_name
    			   }
                    ) stat
    return inh



