module Control.Student.CGI where

--  $Id$

import Control.Types
import Inter.CGI
import Inter.Crypt
import Control.Student.Type as T
import Control.Student.DB

import Control.Monad

login :: Form IO Student
login = do
    click    <- submit    "log" "Login:"
    plain "Matrikel:"
    Just mnr <- textfield "mnr" ""
    plain "Passwort:"
    Just pwd <- password  "pwd" ""
    when click blank
    [ stud ] <- io $ Control.Student.DB.get $ fromCGI mnr
    guard $ Inter.Crypt.compare ( passwort stud ) pwd
    return stud

