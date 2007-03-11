module Control.Schule.CGI where

import Control.Types
import Gateway.CGI
import Control.Schule.Typ as S
import Control.Schule.DB
import Control.Monad


-- | wenn zweites Arg = Nothing, dann erstes nicht benutzen!
edit :: UNr -> Maybe Schule -> Form IO ()
edit u mv = do
    open btable    
    let dtf0 label select =
           defaulted_textfield label $ case mv of
                Just v -> select v ; Nothing -> "?"
        dtf label select = dtf0 label ( toString . select )
        dta0 label select =
           defaulted_textarea label $ case mv of
                Just v -> select v ; Nothing -> ""
    n <- dtf "Name" S.name
    f <- dtf "Mail_Suffix" S.mail_suffix
    close -- btable
    up <- submit "update"
    when up $ do
        io $ put ( fmap S.unr mv ) 
	   $ Schule { unr = u
		       , name = fromCGI n
		      , mail_suffix = fromCGI f
		       }
