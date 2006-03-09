module Control.Vorlesung.CGI where

import Control.Types
import Gateway.CGI
import Control.Vorlesung.Typ as T
import Control.Vorlesung.DB
import Control.Monad

edit :: UNr -> Maybe Vorlesung -> Form IO ()
edit u mv = do
    open btable    
    let dtf0 label select =
           defaulted_textfield label $ case mv of
                Just v -> select v ; Nothing -> "?"
        dtf label select = dtf0 label ( toString . select )
        dta0 label select =
           defaulted_textarea label $ case mv of
                Just v -> select v ; Nothing -> ""
    n <- dtf "Name" T.name
    v <- dtf "einschreiben von" T.einschreibVon
    b <- dtf "einschreiben bis" T.einschreibBis
    m <- dta0 "message of the day" T.motd
    close -- btable
    up <- submit "update"
    when up $ do
        io $ put ( fmap T.vnr mv ) 
	   $ Vorlesung { unr = u
		       , vnr = case mv of 
			     Just v -> vnr v ; Nothing -> undefined
		       , name = fromCGI n
		       , einschreibVon = fromCGI v
		       , einschreibBis = fromCGI b
                       , motd = m
		       }
