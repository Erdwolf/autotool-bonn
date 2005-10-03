module Control.Student.CGI where

--  $Id$

import Control.Types
import Inter.CGI
import Inter.Crypt
import Control.Schule as U
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Schule

import Control.Monad
import Data.List ( partition )
import Data.Char ( isAlphaNum )
import Data.Maybe ( isNothing )

import Autolib.Util.Zufall

login :: Form IO Student
login = do
    -- click    <- submit    "Login:"

    open btable
    us <- io $ Control.Schule.get 
    u <- click_choice "Schule" $ do
        u <- us
	return ( toString $ Control.Schule.name u , u )
    mnr <- defaulted_textfield "Matrikel" ""
    pwd <- defaulted_password  "Passwort" ""
    open row
    click <- submit "Login"
    close -- row
    close -- btable

    when click blank
    studs <- io $ Control.Student.DB.get_unr_mnr 
		        ( U.unr u , fromCGI mnr )
    -- close -- row

    case studs of
         [ stud ] ->
             if Inter.Crypt.compare ( passwort stud ) pwd
                then return stud
                else do
                     plain "Passwort falsch."
                     mzero
         [ ] -> do
             plain "Account existiert nicht."
             mzero

-----------------------------------------------------------------------

edit :: Student -> Form IO ()
edit s = do
    edit_create $ Just s
    return ()

is_a_word :: Monad m => String -> String -> Form m ()
is_a_word label cs = do
    when ( null cs ) $ complain [ label , "leere Eingabe ist nicht erlaubt." ]
    let ( good, bad ) = partition isAlphaNum cs
    when ( not $ null bad ) $ do
	 complain [ label , "diese Zeichen sind nicht erlaubt:", show bad ]

is_an_email :: Monad m => String -> String -> Form m ()
is_an_email label cs = do
    when ( null cs ) $ do
         complain [ label , "leere Eingabe ist nicht erlaubt." ]
    let ( ats, rest ) = partition ( == '@' ) cs
    when ( 1 /= length ats ) $ do
	complain [ label , "Adresse soll genau ein '@' enthalten" ]
    let ok c = isAlphaNum c || c `elem` ".-_"
    let ( good, bad ) = partition ok rest
    when ( not $ null bad ) $ do
	 complain [ label , "diese Zeichen sind nicht erlaubt:", show bad ]

complain css = do
    open row
    sequence_ $ do
        cs <- css
	return $ do
	    plain cs
    close -- row
    mzero    

-- | falls 'Just Student', dann editieren
-- falls 'Nothing', dann anlegen
edit_create :: Maybe Student -> Form IO ()
edit_create ms = do
    open btable
    let dtf label select = 
           defaulted_textfield label $ case ms of
                Just s -> toString $ select s ; Nothing -> ""
    
    us <- io $ Control.Schule.get 
    u <- click_choice "Schule" $ do
        u <- us
	return ( toString $ Control.Schule.name u , u )

    mnr <- dtf "matrikel" T.mnr
    vorname <- dtf "vorname" T.vorname
    name <- dtf "name" T.name
    email <- dtf "email" T.email 
    
    p <- io $ pass
    pw <- defaulted_textfield "password" $ case ms of
       Just _ -> ""
       Nothing -> p

    open row
    check <- submit "check"
    close -- row

    is_a_word "Matrikel" mnr    
    is_a_word "Vorname" vorname
    is_a_word "Name" name
    is_an_email "Email" email

    c <- if null pw 
	 then case ms of
	      Nothing -> complain [ "Passwort", "darf nicht leer sein" ]
	      Just s  -> return $ T.passwort s
	 else do
	      is_a_word "Passwort" pw
	      io $ encrypt pw
    open row
    up <- submit "update"
    close -- row
    close -- btable

    schon <- io $ get_unr_mnr ( U.unr u , fromCGI mnr )
    when ( not $ null schon ) $ do
	plain "diese Matrikelnummer ist bereits in Benutzung"
	mzero

    when up $ do
        io $ Control.Student.DB.put ( fmap T.snr ms )
	   $ T.Student { T.mnr = fromCGI mnr
               , T.unr = case ms of
                     Just s -> T.unr s
                     Nothing -> U.unr u
	       , T.vorname = fromCGI vorname
	       , T.name = fromCGI name
	       , T.email = fromCGI email
               , T.passwort = c
	       }
        plain "update ausgeführt"


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
           then do submit "new password:" ; return True
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


