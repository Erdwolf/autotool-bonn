module Control.Semester.CGI where

import Control.Types
import Gateway.CGI
import Control.Semester.Typ as S
import Control.Semester.DB
import Control.Monad


-- | wenn drittes Arg = Nothing, dann zweites nicht benutzen!
edit :: UNr -> ENr -> Maybe Semester -> Form IO ()
edit u e mv = do
    open btable    
    let dtf0 label select =
           defaulted_textfield label $ case mv of
                Just v -> select v ; Nothing -> "?"
        dtf label select = dtf0 label ( toString . select )
        dta0 label select =
           defaulted_textarea label $ case mv of
                Just v -> select v ; Nothing -> ""
    n <- dtf "Name" S.name
    v <- dtf "beginnt" S.von
    b <- dtf "endet" S.bis
    close -- btable
    up <- submit "update"
    when up $ do
        io $ put ( fmap S.enr mv ) 
	   $ Semester { unr = u
		      , enr = e
		       , name = fromCGI n
		       , von = fromCGI v
		       , bis = fromCGI b
		       }
