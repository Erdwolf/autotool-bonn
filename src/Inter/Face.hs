module Main where

-- $Id$

--
-- Inhalt:
--
-- einfaches cgi-Interface zum Autotool, 
-- keine komplexe Zustandsfolge wie bei wash,
-- sondern jedesmal kompletter Neustart. 
-- Gedaechtniss ist Parameter s.u.
--
import Network.CGI
import Text.Html hiding ( text  )
import ToDoc (Doc, render, toDoc)

import Reporter
import qualified Output
import qualified Output.Html

-- CGI Env-Variabeln in Parameter umwandeln
import Inter.Env

--
-- hier drin ist : 
-- - Passwort Kontrolle
-- - Aufgaben-Varianten abh. von Login bestimmen
-- - vergleichen der Variante mit eingegeben Wunsch
-- 	- Fehler -> mgl Varianten des Logins anzeigen
--	- ok -> Parameter mit Variante bestuecken
--
import Inter.Validate

import Inter.Evaluate
import Inter.Bank

--
-- der Monster-Parameter darin sind alle wesentlichen Daten
-- fuer die aktuelle Session:
--
-- Param { matrikel , problem , aufgabe , version 
-- , passwort , input , ident , input_width 
-- , variants , variante , highscore , anr }
--
import qualified Inter.Param as P

import qualified Challenger

import Inter.Types
import qualified Exception

-- hier sind die aufgaben drin:
import Inter.Boiler

import qualified Posix

main :: IO ()
main = do

     -- damit die Datei-Geschichten funktionieren
     user <- Posix.getEffectiveUserName
     Posix.setEnvVar "HOME" $ "/home/" ++ user

     vs <- boiler
     wrapper $ \ env -> 
	 iface vs env
	 `Exception.catch` \ err -> return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

iface :: [ Variant ] -> [(String, String)] -> IO Html
iface variants env = do

    -- alle Inputs aus Env. holen
    let par0 = 
        -- erste Variante ist Default Variante
		case head variants of 
			Inter.Types.Variant var -> 
			-- default Parameter füllen, bekommt man beim start
				let def = P.empty  { P.problem = show ( Inter.Types.problem var )
							   , P.aufgabe = Inter.Types.aufgabe var
							   , P.version = Inter.Types.version var
							   }
				in Inter.Env.get_with env def

    -- gültigkeit prüfen, aufg.-instanz generieren / bzw. holen
    --
    -- das ist eine IO-Aktion 
    -- (wg. DB-zugriffen und evtl. Random in Generierung)
    -- im Reporter.Type gibts (mit Absicht) (leider) keine IO
    res <- validate $ par0 { P.variants = variants }

    -- haben es wir geschaft zum aufgabenlösen
    case res of
     -- nein, fehler (=msg) : entweder passwort falsch 
     -- oder ungültige Aufgabe (-> zeige mögliche)
     Left msg -> do
          return $ page par0{ P.variants = variants }  msg

     -- ja,
     Right par1 -> case P.variante par1 of
       Variant v -> do


          -- key (für persönliche Aufgabe) und Aufgaben-Instanz
          -- herstellen und anzeigen
          k <- key  v $ P.matrikel par1
          generator <- gen v k
          let ( Just i, com :: Doc ) = export generator

          let inst = p << "Die Aufgabenstellung ist:"
					  +++ p << pre << render com 
          
          -- Beispiel Eingabe holen  
          let b0 = Challenger.initial ( problem v ) i

          -- Eingabe.Form füllen entweder mit
          let par2 = par1 { P.input = 
                            if null ( P.input par1 )
                            -- dem Beispiel oder
                            then show b0 
                            -- der letzten Eingabe
                            else P.input par1
                          }

          -- FIXED BUG: "Falsch Buchung zum Anfang" 
		  -- bin gerade dabei...
          let isFirstRun = null ( P.input par1 ) -- P.input par2  /= show b0

          -- eingabe bewerten ( echter Reporter )
          let ( res :: Maybe Int , com :: Doc ) 
				  = export $ evaluate ( problem v ) i par2
          let ans = 
				  if isFirstRun
				  then noHtml
				  else
				      p << "Das Korrekturprogramm sagt:"
							+++ p << pre << render com 


          -- bewertung in datenbank und file
          log <- if isFirstRun 
				  then return noHtml
	              else 
				      do 
					  msg <- bank par2 res 
					  return $ p << "Eintrag ins Logfile:" +++ p << pre << msg


		  -- Höhe der Eingabe-Form berechen
          let height = length $ filter ( == '\n' ) $ P.input par2

		  -- bewertung ausgeben, bzw. zur Lösungseingabe auffordern
          let status = case res of
					   Just s -> 
						   p << bold << ( "Korrekte Lösung, Size: " ++ show s )
					   Nothing -> 
						   p << bold << ( 
								 if isFirstRun 
								 then "Hier Lösung eingeben:"
								 else "Lösung ist nicht korrekt. Nochmal:" 

								 )
						   +++ textarea ( primHtml $ P.input par2  ) 
								! [ name "input"
								  , rows $ show $ height + 2
								  , cols $ show $ P.input_width par2
								  ]
						   +++ br 
						   +++ submit "submit" "submit" +++ " " 
						   +++ submit "Beispiel" "Beispiel"


          return $ page par2 $ inst +++ log +++ status +++ ans

------------------------------------------------------------------------
--
-- Die HTML Seite und ihre Bestandteile
--
page par msg = 
	let 
	heading = h2 << "Autotool CGI Inter.Face"
	pref = preface par 
	var = varselector par 
	sub = submit "change" "change" 
	chg = "  Achtung, verwirft Lösung!!"
    in  header << thetitle << "Inter.Face"
            +++ body ( form ( heading +++ hr +++ pref +++ sub +++ chg +++ hr +++ msg )
					   ! [ Text.Html.action "Face.cgi"
						 , method "POST" ]
					 )
         
preface par = 
	let var = varselector par in
    table << 
		  aboves [ besides $  
				   txtf' "10" "matrikel" ( P.matrikel par )
				   ++ pwdf "passwort" ( P.passwort par )
				 , besides $ var ++ [ td ! [ colspan 2 ] << "Bitte hier wählen ODER unten eingeben." ]
--				 , besides [ td << h4 << stringToHtml "Quick Start" ]
				 , besides $  
				   txtf' "10" "problem" ( P.problem par )
				   ++ txtf' "5" "aufgabe" ( P.aufgabe par )
				   ++ txtf' "5" "version" ( P.version par )
				 ]

varselector par =
    let 
    vars = [ stringToHtml "-" ] ++ (  map docToHtml ( P.variants par ))
    in
    [ td << "nr "  ,  td << menu "wahl" vars ]

docToHtml = stringToHtml.render.toDoc

txtf name cont = 
    [ td << name , td << textfield name ! [ value cont ] ]

txtf' sz name cont = 
    [ td << name , td << textfield name ! [ value cont , Text.Html.size sz ] ]

pwdf name cont = 
    [ td << name , td << password name ! [ value $ show  cont , Text.Html.size "10" ] ]

