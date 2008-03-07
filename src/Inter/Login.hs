{-# OPTIONS -fglasgow-exts #-}

--  $Id$

module Inter.Login where

import Gateway.CGI
import Control.Types ( VNr, toString, TimeStatus (..) )
import Control.Monad ( when, mzero )

import qualified Control.Vorlesung as V
import qualified Control.Semester
import qualified Control.Vorlesung
import qualified Control.Student.CGI
import qualified Control.Student.Type as S
import qualified Control.Admin.CGI
import qualified Control.Direktor.CGI
import qualified Text.XHtml

import Autolib.Util.Sort

import Data.Typeable

data Status = Student | Tutor | Direktor | Minister
   deriving ( Show, Eq, Ord, Typeable )

-- | returns ( s, v, ist_tutor, ist_eingeschrieben )
-- unterschiede: tutor darf "alles",
-- student darf keine aufgaben ändern und nur aktuelle aufgaben sehen
form :: Form IO ( S.Student, V.Vorlesung, Status , Bool )
form = do

    -- open btable
    stud <- Control.Student.CGI.login
    -- close -- btable

    Control.Admin.CGI.main stud
    Control.Direktor.CGI.main stud

    aule stud

aule stud = do
    sems0 <- io $ Control.Semester.get_at_school $ S.unr stud
    let sems = sortBy ( \ s -> Control.Semester.status s /= Current ) 
	     $ sems0

    if null sems
       then do
           plain "Für diese Schule wurden noch keine Semester definiert."
	   mzero
       else continue stud sems

continue stud sems = do 
    let snr = S.snr stud
    open btable
    sem <- click_choice_with_default 0 "Semester" $ do
	 sem <- sems
	 return ( toString $ Control.Semester.name sem, sem )

    -- alle vorlesungen an dieser Schule
    vors0 <- io $ V.get_at_school ( S.unr stud )
    let vors = reverse $ sortBy V.einschreibVon vors0
    -- hierfür ist er tutor:
    tvors <- io $ V.get_tutored snr
    -- hierfür ist er eingeschrieben:
    avors <- io $ V.get_attended snr

    let current v = Control.Vorlesung.enr v == Control.Semester.enr sem

    vor <- click_choice "Vorlesung" $ do
        vor <- filter current vors
        return ( toString $ V.name vor , vor )
    close -- btable

    let motd = toString $ V.motd vor
    when ( not $ null motd ) $ do
        par 
        plain motd
        par

    let status = if V.vnr vor `elem` map V.vnr tvors
		then Tutor
		else Student
	attends = V.vnr vor `elem` map V.vnr avors

    open btable
    when ( status < Tutor ) close -- risky

    return ( stud, vor, status, attends )

