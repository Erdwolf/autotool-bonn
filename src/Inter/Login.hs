--  $Id$

module Inter.Login where

import Inter.CGI
import Control.Types ( VNr )

import qualified Control.Vorlesung as V
import qualified Control.Student.CGI
import qualified Control.Student.Type as S

form :: Form IO ( S.Student, VNr, Bool )
form = do

    stud <- Control.Student.CGI.login
    let snr = S.snr stud
 
    tvors <- io $ V.get_tutored snr
    -- unterschiede: tutor darf "alles",
    -- student darf keine aufgaben ändern und nur aktuelle aufgaben sehen
    let tutor = not $ null tvors
    vors <- if tutor 
            then return tvors
	    else io $ V.get_attended snr

    vnr <- selector_submit "vnr" "Vorlesung" 0 $ do
        vor <- vors
        return ( show $ V.name vor , V.vnr vor )

    return ( stud, vnr, tutor )

