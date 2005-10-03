module Control.Gruppe.CGI where

import Control.Types
import Inter.CGI
import Control.Gruppe.Typ as T
import Control.Gruppe.DB
import Control.Monad

edit :: VNr -> Maybe Gruppe -> Form IO ()
edit v mg = do
    open btable    
    let dtf label select =
           defaulted_textfield label $ case mg of
                Just g -> toString $ select g ; Nothing -> "?"
    n <- dtf "Name" T.name
    r <- dtf "Referent" T.referent
    m <- dtf "Plätze" T.maxStudents
    close -- btable
    up <- submit "update"
    when up $ do
        io $ put ( fmap T.gnr mg ) 
	   $ Gruppe { vnr = v
		    , name = fromCGI n
		    , referent = fromCGI r
		    , maxStudents = fromCGI m
		    }
