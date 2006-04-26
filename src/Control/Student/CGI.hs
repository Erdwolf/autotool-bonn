module Control.Student.CGI where

--  $Id$

import Control.Types
import Gateway.CGI
import Inter.Crypt
import Control.Schule as U
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Schule

import Control.Monad
import Data.List ( partition, isSuffixOf )
import Data.Char ( isAlphaNum )
import Data.Maybe ( isNothing )

import Autolib.Util.Zufall
import qualified Debug
import qualified Local


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

    change <- click_choice_with_default 0 "Aktion"
           [ ("Login", False)
           , ( "persönliche Daten ändern", True) 
           ]

    close -- btable

    studs <- io $ Control.Student.DB.get_unr_mnr 
		        ( U.unr u , fromCGI mnr )
    -- close -- row

    stud <- case studs of
         [ stud ] ->
             if Inter.Crypt.compare ( passwort stud ) pwd
                then use_first_passwort stud
                else if Inter.Crypt.compare ( next_passwort stud ) pwd
                then use_next_passwort stud
                else wrong_password stud

         [ ] -> do
             plain "Account existiert nicht."
             mzero

         xs -> do
             plain "Mehrere Studenten mit dieser Matrikelnummer?"
             plain $ show $ map T.snr xs
             mzero

    when change $ do
        Control.Student.CGI.edit stud
    return stud

use_first_passwort stud = 
    if ( Inter.Crypt.is_empty $ next_passwort stud ) 
    then return stud -- ändert sich nichts
    else do
        plain $ unlines 
              [ "Sie hatten eine Email mit einem neuen Passwort erhalten,"
              , "aber Sie haben jetzt Ihr altes Passwort benutzt."
              , "Das Passwort aus der Email wird dadurch ungültig,"
              , "Ihr bestehendes (jetzt benutztes) Passwort bleibt gültig."
              ]
        let neu = stud { T.next_passwort = Inter.Crypt.empty }
        io $ Control.Student.DB.put ( Just $ T.snr stud ) neu
        return neu

use_next_passwort alt = do
    plain "Sie haben Ihr neues Passwort verwendet."
    let neu = alt { T.passwort = T.next_passwort alt
                  , T.next_passwort = Inter.Crypt.empty
                  }
    io $ Control.Student.DB.put ( Just $ T.snr alt ) neu
    plain "Das vorherige ist damit ungültig."
    return neu

wrong_password stud = do
    plain "Passwort falsch."
    par 
    if ( Inter.Crypt.is_empty $ next_passwort stud )
       then ask_pwmail stud
       else plain $ unlines
                  [ "Sie haben eine Email mit einem neuen Passwort erhalten,"
                  , "aber Sie haben dieses neue Passwort noch nicht benutzt."
                  ]
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

is_an_email_for_school :: Monad m => U.Schule -> String -> String -> Form m ()
is_an_email_for_school u label cs = do
    is_an_email label cs
    let suf = toString $ U.mail_suffix u
    when ( not $ isSuffixOf suf cs ) $ do
        complain [ label, "es sind nur Mailadressen gestattet, die auf"
                 , suf, "enden."
                 ]

-----------------------------------------------------------------------

complain css = do
    open row
    sequence_ $ do
        cs <- css
	return $ do
	    plain cs
    close -- row
    open row
    blank
    submit "absenden"
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
    u <- case ms of
        -- Student darf Schule nicht ändern
        Just s -> return $ head $ do
            u <- us
            guard $ U.unr u == T.unr s 
            return u
        Nothing -> click_choice "Schule" $ do
            u <- us
	    return ( toString $ Control.Schule.name u , u )

    mnr <- dtf "Matrikelnummer" T.mnr
    vorname <- dtf "Vorname" T.vorname
    name <- dtf "Nachname" T.name
    email <- dtf "Email" T.email 
    
    is_a_word "Matrikelnummer" mnr    
    is_a_word "Vorname" vorname
    is_a_word "Nachname" name
    is_an_email_for_school u "Email" email

    schon <- io $ get_unr_mnr ( U.unr u , fromCGI mnr )
    let others = case ms of
            Just s -> filter ( \ s' -> T.snr s' /= T.snr s ) schon
            Nothing -> schon
    when ( not $ null others ) $ do
        open row
	plain "diese Matrikelnummer ist bereits in Benutzung"
        close
	mzero

    let stud0 = case ms of
          Just s -> s
          Nothing -> T.Student -- cannot log in
                     { T.passwort = Inter.Crypt.empty
                     , T.next_passwort = Inter.Crypt.empty
                     } 
    let stud = stud0
             { T.mnr = fromCGI mnr
             , T.unr = case ms of
                     Just s -> T.unr s
                     Nothing -> U.unr u
	     , T.vorname = fromCGI vorname
	     , T.name = fromCGI name
	     , T.email = fromCGI email
             -- NOTE: passwords are not set here
             }

    -- password handling
    case ms of
        Nothing -> do -- neuer Account: passwort würfeln und mailen,
            open row
            submit "Account ..."
            click <- submit "... anlegen?"
            close -- row
            close -- table
            when click $ do
                io $ Control.Student.DB.put Nothing stud
                [ stud ] <- io $ Control.Student.DB.get_unr_mnr 
                            ( T.unr stud , T.mnr stud )
                pwmail $ stud
            mzero -- never returns

        Just s -> do -- bestehender Account: passwort ändern 
           pw <- defaulted_textfield "password" ""
           c <- if null pw 
	        then do 
                    plain "(keine Eingabe: Passwort wird nicht geändert)"
                    return $ T.passwort s
	        else do
	            is_a_word "Passwort" pw
	            io $ encrypt pw
           open row
           up <- submit "update"
           close -- row
           when up $ do
                io $ Control.Student.DB.put ( Just $ T.snr s )
                   $ stud { T.passwort = c
                          , T.next_passwort = Inter.Crypt.empty
                          }
                plain "update ausgeführt"

    close -- btable

-------------------------------------------------------------------------

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

-- | ask if user wants new password,
-- if yes, then generate and mail,
-- then stop
ask_pwmail stud = do
    open row
    plain $ unlines
          [ "Ein neues Passwort erzeugen und per email zustellen?"
          -- , toString $ email stud
          ]
    click <- submit "Ja."
    close -- row
    when click $ pwmail stud
    mzero

-- | generate new password,
-- put ciphertext in db
-- send plaintext to known email address
-- be very very careful not to allow shell code injection
pwmail stud = do
    let e = toString $ email stud
    is_an_email "Email" e
    let m = toString $ mnr stud
    is_a_word "Matrikel" m    

    p <- io $ pass
    is_a_word "Passwort" p    
    c <- io $ encrypt p
    io $ Control.Student.DB.put ( Just $ T.snr stud )
       $ stud { T.next_passwort = c }

    let echo = texter
            [ "Sie haben ein neues Passwort"
            , "für das E-Learning-System autotool angefordert."
            , unwords [ "Es lautet:", "Matrikelnummer:", m, "Passwort:", p ]
            , "Es wird durch seine erste Benutzung aktiviert,"
            , "Sie können es danach ändern."
            , "Sie können aber auch Ihr bisheriges Passwort weiter benutzen"
            , "und diese Mail ignorieren."
            ]

    let cmd = unwords
           [ echo
           , "|"
           , "/usr/bin/mail"
           , "-s", show "neues autotool-passwort"
           , "-a", show "From: autotool"
           , e
           ]
    when Local.debug $ pre $ "running: " ++ cmd
    res <- io $ Debug.system cmd
    when Local.debug $ pre $ "Exit code: " ++ show res

    pre $ unwords
        [ "Ein neues Passwort wurde an Ihre Mail-Adresse"
        -- , e
        , "gesendet."
        ]

    return ()

-- | writes text using echo shell commands (ugly ugly)
texter :: [ String ] -> String
texter lines = 
    let parens cs = "( " ++ cs ++ " )"
        handle line = " echo " ++ line ++ " ; "
    in  parens $ unlines $ map handle lines

----------------------------------------------------------------
 
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


