module Main where

import Wash.HTMLMonad 
import Wash.CGI

import IO
import Data.Char ( toLower )
import qualified Exception
import System -- system

import Control.Monad ( guard )
import Data.Maybe

-- autoan-modules
import HTMLshortcuts
-- import SQLqueries
import Control.Queries
import Control.Types
import Control.SQL (logged)
import Helper

-- | TODO 
-- SQL-Exception fangen
-- Seiten Struktur rausziehen: stdpage ttl bdy menu
main :: IO ()
main =  run [] ( loginPage "" F0 ) --mainCGI 
     `Exception.catch` \ ex -> putStrLn $ "\n\n" ++ show ex 



msgPasswort = do 
        empty

-- ---------
-- Einstiegseite
-- mit folgen Moeglichkeiten
-- a) Neuanmeldung:             registerPage 
-- b) Normaler Login:           checkLoginPage

loginPage (mat :: String) F0  = do
        io $ logged "loginPage"
        mglVorlesungen <- io getAllVorlesungenDB                        
        io $ logged $ "mglVorlesungen: " ++ show mglVorlesungen
        -- Vorlesungen zusammen kleben also [a,b,c] -> "a, b, c"
        let { kleber a "" = a -- ignoriere leere Vorlesung ""
                ; kleber a b = a ++ ", " ++ b
                ; vorlesungen = foldr1 kleber mglVorlesungen }
        standardQuery "Willkommen." $ do 
                table $ do 
                        attr "width" "600"
                        hrrow
                        th3 "Login"
                        spacerow
                        ttxt $ concat 
                                         [ "Wenn Sie bereits angemeldet sind, Matrikelnr. und Passwort eingeben." 
                                         ]
                        ttxt $ "Dann können Sie Ihre Daten ändern/ansehen."
                        spacerow
                        matF <- promptedInput           "Matrikelnr:"   $ (fieldSIZE 30) ## ( fieldVALUE mat )
                        pwdF <- promptedPassword        "Passwort:"             (fieldSIZE 30)

                        -- a)
                        smallSubButton (F2 matF pwdF)   checkLoginPage  "Login" 
                        -- c)

                        hrrow
                        th3 $ "Hier können Sie sich in die Übungsgruppen von " ++ vorlesungen ++ " einschreiben."
                        spacerow
                        ttxt "Dazu müssen Sie sich neu anmelden und dann eine Gruppe wählen."
                        spacerow
                        -- b)
                        smallSubButton F0 (registerPage "" "" "" "") "Neuanmeldung"



-- ----------------
-- Neuanmeldung, Eingabe der persoenlichen Daten mit folgender Validierung (registerPage). 
-- 
-- Hinweis,
-- beim ersten Aufruf alle Parameter mit Leeren-Strings, sonst mit bereits eingegeben Werten.
--
registerPage vnm nme ( mat :: String ) eml F0 = 
    standardQuery "Willkommen zur Neuammeldung" $ do
        table $ do      
           attr "width" "600"      
           hrrow
           th3 "Nach der Anmeldung können Sie die Vorlesung/Übungsgruppe auswählen."
           spacerow
           ttxt "Bitte achten Sie auf die korrekte Angabe von Matrikelnummer und Email-Adresse."
           spacerow
           ttxt $ "Notieren Sie bitte Ihr Passwort." 
           spacerow
           vnmF <- promptedInput   "Vorname"       $ (fieldSIZE 30) ## (fieldVALUE vnm )
           nmeF <- promptedInput   "Name"          $ (fieldSIZE 30) ## (fieldVALUE nme )
           matF <- promptedInput   "Matrikelnr."   $ (fieldSIZE 30) ## (fieldVALUE mat )
           emlF <- promptedInput   "Email-Adresse" $ (fieldSIZE 30) ## (fieldVALUE eml )
           spacerow
           msgPasswort
           ps1F <- promptedPassword "Passwort"             (fieldSIZE 30) 
           ps2F <- promptedPassword "Passwort Wdh." (fieldSIZE 30)
           hrrow
           smallSubButton (F6 vnmF nmeF matF emlF ps1F ps2F) registeredPage "Anmelden"
           smallSubButton F0 endPage "Ende"

--
-- Überprüfung der Registration
--
-- Bei Fehler (siehe Variable checks) 
-- Aufruf der registeredPage (a) mit bereits eingegeben Werten fuer Korrektur, 
-- sonst normal weiter mit der Gruppenauswahl (b).
registeredPage (F6 vnmF nmeF matF emlF ps1F ps2F) = 
        let 
                vnm = unNonEmpty                ( value vnmF )
                nme = unNonEmpty                ( value nmeF )

                mat0 = unAllDigits              ( value matF )
                mat = fromCGI mat0 :: MNr

                eml = unEmailAddress    ( value emlF )
                ps1 = unNonEmpty                ( value ps1F )
                ps2 = unNonEmpty                ( value ps2F )
                matok = if allDigits mat0 then (read mat0) > 1024 else False
        in
        do 
        -- suche nach dubletten von email or mat
        ( matDup , emlDup ) <- io $ duplMatOrEmailDB mat eml
        -- Diese Tests muessen alle bestanden werden, um Registration abzuschliessen.
        let { checks = [ ( ps1 == ps2                   , "Passwort und Passwort Wiederholung nicht identisch.")
                         -- Q: Warum kann mat hier nicht direkt checken?                                         
                                   , ( matok                            , "Die Matrikelnummer muss größer 1024 sein.")
                                   , ( length vnm > 1           , "Der Vorname muss mindestens 2 Buchstaben lang sein.") 
                                   , ( length nme > 1           , "Der Name muss mindestens 2 Buchstaben lang sein.") 
                                   , ( not matDup                       , "Matrikelnummer bereits registiert.")
                                   , ( not emlDup                       , "Email bereits registiert.")
                                   ]
                ; allok = and [ fst c | c <- checks ]
                }
        -- Wenn Test ok: Student in DB eintragen
        if allok then io $ insertNewStudentDB vnm nme mat eml ps1 else return ()
        standardQuery "Neuanmeldung" $ table $ do
            attr "width" "600"      
            hrrow
            if allok 
               then do -- (b) Erfolgreich
                        th3 "Vielen Dank. Alle Angaben wurden übernommen. "
                        -- msgKontrollEmail
                        hrrow
                        smallSubButton F0 (changeGrpPage mat) "Weiter"
               else do -- (a) Ausgabe der nicht bestanden Tests
                        ttxt "Bitte überprüfen Sie Ihre Angaben:"
                        spacerow
                        mapM_ (ttxt) [  msg | ( ok , msg ) <- checks , not ok ]
                        hrrow
                        smallSubButton F0 (registerPage vnm nme mat0 eml) "Zurück"

-- ----------------
-- Endseite 
endPage F0 = 
        standardQuery "Ende" $ 
                table $ do      
                        hrrow
                        th3 "Vielen Dank für die Benutzung. Und schönen Tag noch."
                        hrrow


-- ----------------
-- Überpüfung der passwort,matrikel paares, bei Erfolg Student-Status anzeigen
checkLoginPage (F2 matF pwdF) =
  let 
                mat0  = unAllDigits ( value matF )
                mat = fromCGI mat0 :: MNr
                pass = unNonEmpty ( value pwdF )
  in
        do inh <- io $ checkPasswdMNrDB (Just pass) mat
           if  ( (length inh)  == 0  ) -- passwort nicht okay?
                 then 
                        do standardQuery "Fehler:" $ 
                                table $ do
                                        hrrow           
                                        th3 "Passwort und Matrikelnummer stimmen nicht überein!" 
                                        hrrow
                                        smallSubButton F0 (loginPage mat0)                               "Login wiederholen"
                                        smallSubButton F0 (registerPage "" "" "" "")       "Neuanmeldung"
                                        smallSubButton F0 endPage                                               "Ende"

                 else 
                        do studStatusPage mat F0

--
-- | Hauptseite fuer den Studenten
--
-- Mit folgenden Informationen 
-- a) Vorname, Name, Matrikelnr, Email-Adresse, Email-Check-Status, 
--    Gruppen/Vorlesungen des Studenten
-- b) momentan mgl. Aufgaben
-- c) Bepunktung der Aufgaben
--
-- und Aenderungs Moeglichkeiten 
-- d) Gruppenmitgliedschaft:    changeGrpPage
-- e) Email-Adresse:                    changeEmailPage
-- f) Passwort:                                 changePasswortPage
--
-- und (Zucker)
-- g) Aktualisieren der Ansicht: studStatusPage
-- h) Beendigung von autonan:    endPage
--
-- TODO: mglNextAufgabenDB [ ] vom SNr abhaengen lasse
studStatusPage ( mat :: MNr ) F0 = 
        do 
          io $ logged $ "studStatusPage"
          snr                   <- io $ getSNrFromMatDB mat 
          io $ logged $ "snr: " ++ show snr
          -- a) Persoenliche Daten aus DB holen 
          inh                   <- io $ checkPasswdMNrDB Nothing mat
          io $ logged $ "inh: " ++ show inh

          let ( vorname , name , email , status ) = ( inh !! 0 )        
          -- Student ist Mitglied in folgenden Gruppen:
          --    Alle-Gruppen minus Students-Gruppen, warum so kompliziert?
          --    stdgrp ist Liste von Gruppennummern, grps enthaelt zusaetzlich 
          --    Vorlesungsname, Gruppenname und Referent.
          (h,grps)              <- io $ getAllGruppenDB 
          io $ logged $ "grps: " ++ show grps

          stdgrp        <- io $ getGruppenStudDB mat
          io $ logged $ "stdgrp: " ++ show stdgrp

          let   grp = filter ( \ (g,_) -> g `elem` stdgrp ) grps

          -- b) Mgl. Aufgaben aus DB holen, und erste Spalte entfernen 
          mglAufgs      <- io $ mglNextAufgabenDB (snr!!0) 
          io $ logged $ "mglAufgs: " ++ show mglAufgs

          let mglAufgs2 = ( tail (fst mglAufgs) , Prelude.map tail (snd mglAufgs))
          -- c) Bepunktete Aufgaben holen
          result                <- io $ studAufgDB $ Just mat
          -- gesamtres <- io $ getSerienPunkteDB mat
          standardQuery "Übersicht" $ 
                do 
                        hrline
                        h3 $ text "Anmeldungsdaten"
                        table $ 
                                do 
                                attr "cellspacing" "2" 
                                attr "cellpadding" "5"
                                -- (a)
                                tableRow2 (text "Name") ( text ( vorname ++ ", " ++ name ++ " " ) )
                                tableRow2 (text "Matrikelnr.")   ( text $ show mat )
                                tableRow2 (text "Email-Adresse") ( text email )
                                tableRow2 (text "Übungsgruppen") ( grptab grp )

                        -- gesamtergTable gesamtres
                                -- (c)
                        if ( length (snd result)) > 0 
                           then do { h3 $ text "Ergebnisse" ; ( showAsTable result ) }
                           else do { h3 $ text "keine Ergebnisse" } 
                        spacerow
                        -- (b)
                        th3 "Moegliche Aufgaben:"
                        if null (snd mglAufgs2) 
                           then tr $ td $ text "zur Zeit keine."
                           else showAsTable2 mglAufgs2
                        -- --------------------------------------------------
                        hrline
                        h3 $ text "Hier können Sie :"
                        table $ 
                                do 
                                   smallSubButton F0 (changeGrpPage mat)                        "Vorlesung/Übungsgruppe auswählen/ändern"
                                   smallspacerow
                                   smallSubButton F0 (changeEmailPage mat email)        "die Email-Adresse ändern"
                                   smallSubButton F0 (changePasswortPage mat)           "das Passwort ändern"
                                   smallspacerow
                                   smallSubButton F0 (studStatusPage mat)                       "Übersicht aktualisieren"
                                   smallspacerow
                                   smallSubButton F0 endPage                                            "beenden"
                where 
                gesamtergTable daten = 
                    do 
                    hrline
                    h3 $ text $ concat ["Gesamtpunkte:" ," " , show $ summe daten ] --," von 73 mgl." ]
                    table $ mapM_ lin daten
                    hrline
                    where  lin (a,b) = tr $ do { td $ text a; td $ text b  }
                           summe = sum . Prelude.map readsnd
                           readsnd :: (String,String) -> Int 
                           readsnd = read . snd

------------------------------------------------------------------------------------
-- TODO gruppen wechseln nach Bepunktung verhindern
changeGrpPage mat F0 = do
          (h,freegrps)  <- io $ getFreeGruppenDB 
          (h,allgrps)   <- io $ getAllGruppenDB
          stdgrp        <- io $ getGruppenStudDB mat
          let   
                posgrp =  filter ( \ (g,_) -> not $ g `elem` stdgrp ) freegrps
                currentgrps = filter ( \ (g,_) -> g `elem` stdgrp ) allgrps
                thrd (_,_,x) = x
          let { show' = show . snd }
          standardQuery "Übungsgruppe ändern" $ do
                  h3 $ text "Sie besuchen diese Übungsgruppen:" 
                  if null currentgrps then text "keine" else do
                      grptab currentgrps 
                      ttxt "Eine Übungsgruppe verlassen:"
                      vgrpF <- selectSingle show' Nothing currentgrps empty
                      smallSubButton (F1 vgrpF) (leaveGrpPage mat) "Weiter"

                  h3 $ text "Noch freie Übungsgruppen sind:"
                  if null posgrp then text "keine" else do
                      grptab posgrp 
                      ttxt "Eine Übungsgruppe besuchen:"
                      grpF <- selectSingle show' Nothing posgrp empty
                      smallSubButton (F1 grpF) (changedGrpPage mat) "Weiter" 

                  hrrow
                  smallSubButton F0 (studStatusPage mat) "Zurück"

grptab grps = mytable $ do
    tableRow3 [ text "Vorlesung", text "Gruppe", text "Dozent" ]
    sequence $ do
        grp @ (gnr, [v,g,r]) <- grps
        return $ tableRow3 [ text v, text g, text r ]

changedGrpPage = commonGrpPage "ausgewählt" changeStudGrpDB 
leaveGrpPage   = commonGrpPage "verlassen"  leaveStudGrpDB 

commonGrpPage name action  mat (F1 grpF) = do 
        let (grp,desc) = value $ grpF
        io $ action mat grp   
        let msg = unwords [ "Übungsgruppe" , show desc, name ]
        standardQuery msg $ table $ do
                        attr "width" "600"
                        hrrow
                        ttxt $ msg
                        hrrow
                        smallSubButton F0 (studStatusPage mat) "Weiter" 

--------------------------------------------------------------------------------------

-- Email - Änderung
changeEmailPage mat email F0 =
        standardQuery "Änderung der Email-Adresse" $ do
                table $ do 
                        attr "width" "600"
                        hrrow
                        emailF <- promptedInput "Neue Email-Adresse" (fieldVALUE email)
                        hrrow
                        smallSubButton (F1 emailF) (changedEmailPage mat email) "Ändern"  
                        smallSubButton F0 (studStatusPage mat) "Zurück"
        
changedEmailPage mat oldemail (F1 emailF)       = do 
        let email = unEmailAddress $ value emailF
        do
         if (oldemail == email) 
           then do
                        standardQuery "Fehler: Keine Änderung" $ do
                                table $ do
                                        attr "width" "600"
                                        hrrow
                                        ttxt "Die Email-Adresse wurde nicht geändert."
                                        hrrow
                                        smallSubButton F0 (studStatusPage mat) "Weiter"
                else do         
                        io $ updateEmailDB mat email
                        standardQuery "email" $ do
                                table $ do 
                                        attr "width" "600"
                                        hrrow
                                        tableRow2       ( text "Email-Adresse geändert auf ") (text email)
                                        hrrow
                                        smallSubButton F0 (studStatusPage mat) "Weiter"

----------------------------------------------------------------------------------------

-- Passwort - Änderung
changePasswortPage mat F0 =
        standardQuery "Änderung des Passwort" $ do
                table $ do 
                        attr "width" "600"
                        hrrow
                        msgPasswort
                        ttxt ""
                        ttxt "Bitte geben Sie Ihr neues Passwort zweimal ein:"  
                        ttxt ""
                        passwortF <- promptedPassword "Neues Passwort" (fieldSIZE 40)
                        passwort2F <- promptedPassword "Wiederholung" (fieldSIZE 40)
                        hrrow
                        smallSubButton (F2 passwortF passwort2F) (changedPasswortPage mat) "Ändern"  
                        smallSubButton F0 (studStatusPage mat) "Zurück"
        
changedPasswortPage mat (F2 passwortF passwort2F) = 
        let 
                passwort  = unNonEmpty $ value passwortF
                passwort2 = unNonEmpty $ value passwort2F
        in      
          if (passwort2 == passwort) 
                then do         
                         io $ updatePasswortDB mat passwort
                         standardQuery "Passwort" $ do
                                table $ do 
                                        attr "width" "600"
                                        hrrow
                                        ttxt "Passwort geändert."
                                        hrrow
                                        smallSubButton F0 (studStatusPage mat) "Weiter"
                else do
                        standardQuery "Fehler: " $ do
                                table $ do
                                        attr "width" "600"
                                        hrrow
                                        ttxt "Passwort und Wiederholung nicht identisch."
                                        hrrow
                                        smallSubButton F0 (studStatusPage mat) "Weiter"
                 






