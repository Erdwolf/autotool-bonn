module Control.Student.CGI where

--  $Id$

import Control.Types
import Inter.CGI
import Inter.Crypt
import Control.Schule as S
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Schule

import Control.Monad

import Autolib.Util.Zufall

login :: Form IO Student
login = do
    open row
    click    <- submit    "Login:"

    open table
    us <- io $ Control.Schule.get 
    u <- click_choice "Schule" $ do
        u <- us
	return ( toString $ Control.Schule.name u , u )
    open row
    plain "Matrikel:"
    Just mnr <- textfield ""
    close -- row
    open row
    plain "Passwort:"
    Just pwd <- password  ""
    close -- row
    close -- table

    when click blank
    studs <- io $ Control.Student.DB.get_unr_mnr 
		        ( S.unr u , fromCGI mnr )
    close -- row

    case studs of
         [ stud ] ->
             if Inter.Crypt.compare ( passwort stud ) pwd
                then return stud
                else do
                     plain "Passwort falsch."
                     mzero
         [ ] -> do
             plain "Account existiert nicht."
             click <- submit "Account anlegen"
             when click $ do
                 blank -- ??
                 edit_create $ Left u -- für diese Schule jemanden anlegen
             mzero

-----------------------------------------------------------------------

edit :: Student -> Form IO ()
edit s = do
    edit_create $ Right s
    return ()

-- | falls 'Right Student', dann editieren
-- falls 'Left Schule', dann anlegen
edit_create :: Either S.Schule Student -> Form IO ()
edit_create eus = do
    open btable
    let dtf label select = 
           defaulted_textfield label $ case eus of
                Right s -> toString $ select s ; Left _ -> "?"
    mnr <- dtf "matrikel" T.mnr
    vorname <- dtf "vorname" T.vorname
    name <- dtf "name" T.name
    email <- dtf "email" T.email 
    Just c <- generator0 True 
    close -- btable
    up <- submit "update"
    when up $ do
        io $ Control.Student.DB.put 
              ( case eus of Right s -> Just $ T.snr s ; Left u -> Nothing )
	   $ T.Student { T.mnr = fromCGI mnr
               , T.unr = case eus of
                     Right s -> T.unr s
                     Left  u -> S.unr u
	       , T.vorname = fromCGI vorname
	       , T.name = fromCGI name
	       , T.email = fromCGI email
               , T.passwort = c
	       }
        plain "update ausgeführt"
    open btable


-- | generate, display and encode "random" passwort
generator s = do
    mc <- generator0 False
    case mc of
        Nothing -> return ()
        Just c  -> 
           io $ Control.Student.DB.put (Just $ T.snr s)
	      $ s { T.passwort = c }

generator0 must = do
    open row
    gen <- if must 
           then do pre "new password:" ; return True
           else do submit "generate new password?"
    if gen 
       then do
           p <- io $ pass
           pre p -- raw form anzeigen
           c <- io $ encrypt p
           close -- row
           return $ Just c
       else do
           close -- row
           return Nothing

 
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


