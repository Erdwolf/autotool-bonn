module Main where

-- -- $Id$

--
-- Inhalt:
--
-- einfaches cgi-Interface zum Autotool.
--
-- Allgemein:
--  bei jedem CGI-aufruf werden: 
--  - die login-daten neu gepr�ft, 
--  - die instanz neu erzeugt,
--  - die l�sung bewertet, 
--  - und eine textarea f�r die eingabe
--    des n�chsten versuchs generiert.
--
--  keine komplexe Zustandsfolge wie bei wash,
--  sondern jedesmal kompletter Neustart. 
--  (siehe ../doc/haskell-wash.txt)
--  Gedaechtniss ist Parameter s.u.
--
-- Autoren: Johannes Waldmann, Alf Richter
--

-- Standard CGI,HTML lib
import Network.CGI
import Text.Html hiding ( text  )

-- Pretty-Printer Erweiturng
import ToDoc (Doc, render, toDoc, text)

-- Aufgaben Bewertungsprotokoll
import Reporter
import qualified Output
import qualified Output.Html

--
-- CGI Env-Variabeln in Parameter umwandeln
-- Akutellen Zustand berechen.
--
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

--
-- Bewertung einer L�sung in der (Daten)Bank sichern
-- und ins Logs schreiben.
--
import Inter.Bank
import Inter.Store ( latest )

--
-- der Monster-Parameter darin sind alle wesentlichen Daten
-- fuer die aktuelle Session:
--
-- Param { matrikel , problem , aufgabe , version 
-- , passwort , input , ident , input_width 
-- , variants , variante , highscore , anr }
--
import qualified Inter.Param as P
import qualified Inter.Motd

import qualified Challenger

import Inter.Types
import qualified Exception

import Inter.Timer

--
-- hier sind die aufgaben drin:
--
import Inter.Boiler


import qualified Posix
import Informed

patience :: Int
patience = 15 -- seconds

main :: IO ()
main = do
     vs <- boiler
     wrapper $ \ env ->  
         iface vs env
             `Exception.catch` \ err -> 
                 return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

iface :: [ Variant ] -> [(String, String)] -> IO Html
iface variants env = do

    -- alle Inputs aus Env. holen
    let par0 = case head variants of 
        -- erste Variante ist Default Variante
	     Inter.Types.Variant var -> 
	-- default Parameter f�llen, bekommt man beim start
		let def = P.empty  
			{ P.problem = show ( Inter.Types.problem var )
			, P.aufgabe = Inter.Types.aufgabe var
			, P.version = Inter.Types.version var
			}
		in Inter.Env.get_with env def

    motd <- Inter.Motd.contents

    -- g�ltigkeit pr�fen, aufg.-instanz generieren / bzw. holen
    --
    -- das ist eine IO-Aktion 
    -- (wg. DB-zugriffen und evtl. Random in Generierung)
    -- im Reporter.Type gibts (mit Absicht) (leider) keine IO
    res <- validate $ par0 { P.variants = variants }

    -- haben es wir geschafft zum aufgabenl�sen
    case res of
     -- nein, fehler (=msg) : entweder passwort falsch 
     -- oder ung�ltige Aufgabe (-> zeige m�gliche)
     Left msg -> do
          return $ page par0{ P.variants = variants }  ( msg +++ motd )

     -- ja,
     Right par1 -> case P.variante par1 of
       Variant v -> do

          -- key (f�r pers�nliche Aufgabe) und Aufgaben-Instanz
          -- herstellen und anzeigen
          k <- key  v $ P.matrikel par1
          generator <- gen v k
	  -- TODO: das com sollte eigentlich leer sein
	  -- (Beschreibung ist jetzt in report/describe)
          let ( Just i, com :: Doc ) = export generator

          ( _ , desc :: Html ) <- run $ do
		  -- set default dir
	          Challenger.report (problem v) i
          let inst =   h3 << "Aufgabenstellung"
		   +++ p  <<  desc
          
          -- Beispiel Eingabe holen  
          let b0 = Challenger.initial ( problem v ) i

          ein <- case item env "submit" of
	      "example"  -> return $ show b0
	      ""         -> return $ show b0
	      "submit"   -> return $ P.input par1
	      "previous" -> latest par1 
		  `Exception.catch` \ _ -> return ( show b0 )


          -- Eingabe.Form f�llen entweder mit
          let par2 = par1 { P.input = ein }

          -- FIXED BUG: "Falsch Buchung zum Anfang" 
		  -- bin gerade dabei...
          let isFirstRun = null ( P.input par1 ) -- P.input par2  /= show b0

          -- eingabe bewerten ( echter Reporter )
	  
          -- neu (11. 11. 03): IO-Aktion ausf�hren
          ( res :: Maybe Int , com :: Doc ) 
	      <- timed_run patience ( reject $ text "timer expired" ) $ do
	  	         -- TODO: set default dir
	                 evaluate ( problem v ) i par2

          let ans = 
		  if isFirstRun
		  then noHtml
		  else
		      p << "Das Korrekturprogramm sagt:"
					+++ p << pre << render com 


          -- bewertung in datenbank und file
          log <- if isFirstRun 
	      then return noHtml
              else do 
		  msg <- bank par2 res 
		  return $ p << "Eintrag ins Logfile:" +++ p << pre << msg


		  -- H�he der Eingabe-Form berechnen
          let height = length $ filter ( == '\n' ) $ P.input par2

		  -- bewertung ausgeben, bzw. zur L�sungseingabe auffordern
          let status = case res of
		   Just s -> 
			   p << bold << ( "Korrekte L�sung, Size: " ++ show s )
		   Nothing -> 
			   p << bold << ( 
					 if isFirstRun 
					 then "Hier L�sung eingeben:"
					 else "L�sung ist nicht korrekt. Nochmal:" 

					 )
			   +++ textarea ( primHtml $ P.input par2  ) 
					! [ name "input"
					  , rows $ show $ height + 2
					  , cols $ show $ P.input_width par2
					  ]
			   +++ br 
			   +++ submit "submit" "submit" +++ " " 
			   +++ submit "submit" "previous" +++ " " 
			   +++ reset  "submit" "reset" +++ " " 
			   +++ submit "submit" "example"


          return $ page par2 $ inst +++ motd +++ log +++ status +++ ans

------------------------------------------------------------------------
--
-- Die HTML-Seite und ihre Bestandteile
--
page par msg = 
    let 
	heading = h2 << "Autotool CGI Inter.Face"
	pref = preface par 
	var = varselector par 
	sub = submit "change" "change" 
	chg = primHtml "  Achtung, verwirft L�sung!!"
    in  header << thetitle << "Inter.Face"
            +++ body ( form ( foldr1 (+++) 
			       [ heading , hr , pref , sub , chg , hr , msg ] )
				   ! [ Text.Html.action "Face.cgi"
				 , method "POST" ]
		 )
         
preface par = 
	let var = varselector par in
    table << 
	  aboves [ besides $  
   	   txtf' "10" "matrikel" ( P.matrikel par )
   	   ++ pwdf "passwort" ( P.passwort par )
   	 , besides $ var ++ [ td ! [ colspan 2 ] 
			      << "Bitte hier w�hlen ODER unten eingeben." ]
-- 		 , besides [ td << h4 << stringToHtml "Quick Start" ]
   	 , besides $  
   	      txtf' "10" "problem" ( P.problem par )
   	   ++ txtf' "5" "aufgabe" ( P.aufgabe par )
   	   ++ txtf' "5" "version" ( P.version par )
   	 ]

varselector par =
    let 
    vars = map primHtml 
	 $ "--" : map show ( P.variants par )
    in
    [ td << "nr "  ,  td << menu "wahl" vars ]

-- obsolete:
-- docToHtml = stringToHtml.render.toDoc

txtf name cont = 
    [ td << name , td << textfield name ! [ value cont ] ]

txtf' sz name cont = 
    [ td << name , td << textfield name ! [ value cont , Text.Html.size sz ] ]

pwdf name cont = 
    [ td << name , td << password name ! [ value $ show  cont , Text.Html.size "10" ] ]

