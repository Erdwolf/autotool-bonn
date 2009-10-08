module Control.Vorlesung.CGI where

import Control.Types
import Gateway.CGI
import Control.Vorlesung.Typ as T
import Control.Vorlesung.DB
import Control.Time
import qualified Control.Time.CGI as Control.Time
import qualified Control.Semester
import qualified Control.Schule
import Control.Monad

delete :: Control.Schule.Schule 
     -> Vorlesung 
     -> Form IO ()
delete u v = do
    open btable
    open row
    plain $ "Vorlesung löschen:"
    plain $ show v
    close -- row
    close -- btable
    up <- submit "ausführen"
    when up $ do
        io $ Control.Vorlesung.DB.delete $ T.vnr v

edit :: Control.Schule.Schule 
     -> Maybe Vorlesung 
     -> Form IO ()
edit u mv = do
    open btable    
    let dtf0 label select =
           defaulted_textfield label $ case mv of
                Just v -> select v ; Nothing -> "?"
        dtf label select = dtf0 label ( toString . select )
        dta0 label select =
           defaulted_textarea label $ case mv of
                Just v -> select v ; Nothing -> ""
    open row ; plain "Schule" ; plain $ toString $ Control.Schule.name u ; close

    n <- dtf "Vorlesung" T.name
    sem <- pick_semester u $ fmap T.enr mv
    v <- Control.Time.edit "einschreiben von" $ fmap T.einschreibVon mv
    b <- Control.Time.edit "einschreiben bis" $ fmap T.einschreibBis mv
    m <- dtf "message of the day" T.motd
    close -- btable
    up <- submit "update"
    when up $ do
        io $ put ( fmap T.vnr mv ) 
	   $ Vorlesung { unr = Control.Schule.unr u
		       , enr = Control.Semester.enr sem
		       , vnr = case mv of 
			     Just v -> vnr v ; Nothing -> undefined
		       , name = fromCGI n
		       , einschreibVon = v
		       , einschreibBis = b
                       , motd = fromCGI m
		       }

pick_semester  :: Control.Schule.Schule 
	       -> Maybe ENr
	       -> Form IO Control.Semester.Semester
pick_semester u me = do
    sems <- io $ Control.Semester.get_at_school $ Control.Schule.unr u
    let def = 
	   case filter ( \ (k,sem) -> Just ( Control.Semester.enr sem ) == me ) 
		$ zip [ 0 .. ] sems
	   of (k, _) : _ -> Just k
	      []      -> Nothing
    [sem] <- selectors "Semester" [ ( def, do
         sem <- sems
	 return ( toString $ Control.Semester.name sem, sem )
      ) ]
    return sem

