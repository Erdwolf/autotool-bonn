module Control.Student.CGI where

--  $Id$

import Control.Types
import Inter.CGI
import Inter.Crypt
import Control.Student.Type as T
import Control.Student.DB

import Control.Monad

import Autolib.Util.Zufall

login :: Form IO Student
login = do
    click    <- submit    "loglog" "Login:"
    plain "Matrikel:"
    Just mnr <- textfield "logmnr" ""
    plain "Passwort:"
    Just pwd <- password  "logpwd" ""
    when click blank
    [ stud ] <- io $ Control.Student.DB.get $ fromCGI mnr
    guard $ Inter.Crypt.compare ( passwort stud ) pwd
    return stud

edit :: Student -> Form IO ()
edit s = do
    open btable
    mnr <- defaulted_textfield "mnr" $ toString $ T.mnr s
    vorname <- defaulted_textfield "vorname" $ toString $ T.vorname s
    name <- defaulted_textfield "name" $ toString $ T.name s
    email <- defaulted_textfield "email" $ toString $ T.email s
    close -- btable
    up <- submit "up" "update"
    when up $ do
        io $ Control.Student.DB.put (Just $ T.snr s)
	   $ s { T.mnr = fromCGI mnr
	       , T.vorname = fromCGI vorname
	       , T.name = fromCGI name
	       , T.email = fromCGI email
	       }
        plain "update ausgeführt"
    open btable
    generator s


-- | generate, display and encode "random" passwort
generator s = do
    open row
    gen <- submit "gen" "new password"
    when gen $ do
       p <- io $ pass
       pre p
       c <- io $ encrypt p
       io $ Control.Student.DB.put (Just $ T.snr s)
	  $ s { T.passwort = c }
    close -- row

 
vokale = "aeiouy"
konsonanten = "bcdfghjklmnpqrstvwxz"

drei = do
   k <- eins konsonanten
   v <- eins vokale
   j <- eins konsonanten
   return [k,v,j]


zwei = do
   k <- eins konsonanten
   v <- eins vokale
   return [k,v]

pass = do
   ws <- sequence [ drei, zwei, zwei, drei ]
   return $ concat ws


