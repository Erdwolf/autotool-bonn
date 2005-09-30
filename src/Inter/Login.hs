--  $Id$

module Inter.Login where

import Inter.CGI
import Control.Types ( VNr, toString )
import Control.Monad ( when )

import qualified Control.Vorlesung as V
import qualified Control.Student.CGI
import qualified Control.Student.Type as S

form :: Form IO ( S.Student, VNr, Bool )
form = do

    open btable
    stud <- Control.Student.CGI.login
    close -- btable
    let snr = S.snr stud
 
    tvors <- io $ V.get_tutored snr
    -- unterschiede: tutor darf "alles",
    -- student darf keine aufgaben ändern und nur aktuelle aufgaben sehen
    let tutor = not $ null tvors
    vors <- if tutor 
            then return tvors
	    else io $ V.get_attended snr

    open btable
    vnr <- click_choice "Vorlesung" $ do
        vor <- vors
        return ( toString $ V.name vor , V.vnr vor )
    when ( not tutor ) close -- risky

    return ( stud, vnr, tutor )

