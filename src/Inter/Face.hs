-- | Inhalt:
--
-- einfaches cgi-Interface zum Autotool.
--
-- Allgemein:
--  bei jedem CGI-aufruf werden: 
--  - die login-daten neu geprüft, 
--  - die instanz neu erzeugt,
--  - die lösung bewertet, 
--  - und eine textarea für die eingabe
--    des nächsten versuchs generiert.
--
--  keine komplexe Zustandsfolge wie bei wash,
--  sondern jedesmal kompletter Neustart. 
--  (siehe ../doc/haskell-wash.txt)
--  Gedaechtnis ist Parameter s.u.
--
-- Autoren: Johannes Waldmann, Alf Richter

module Main where

--  $Id$


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
-- Bewertung einer Lösung in der (Daten)Bank sichern
-- und ins Logs schreiben.
--
import Inter.Bank
import Inter.Store ( latest )
import Inter.Click

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
import Inter.Make
import qualified Control.Exception


import Data.Typeable


import Inter.Timer
import Inter.Logged
--
-- hier sind die aufgaben drin:
--
import Inter.Boiler


-- temporarily:
import qualified HTWK.SS04.Informatik.Boolean as B

import Informed

patience :: Int
patience = 15 -- seconds

main :: IO ()
main = do
     -- original:
     -- vs <- boiler

     -- testing:
     let vs = get_boiler B.makers B.configs

     wrapper $ \ env ->  
         iface vs env
             `Control.Exception.catch` \ err -> 
                 return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

iface :: [ Variant ] -> [(String, String)] -> IO Html
iface variants env = do

    logged "start" $ return ()

    let pres = h3 << "(experimental:) Makers" 
	    +++ p << pre << show ( present B.makers )
            +++ h3 << "(experimental:) Configs"
	    +++ p << pre << show ( cpresent B.makers B.configs )
		    

    -- alle Inputs aus Env. holen
    let par0 = case head variants of 
        -- erste Variante ist Default Variante
	     Inter.Types.Variant var -> 
	-- default Parameter füllen, bekommt man beim start
		let def = P.empty  
			{ P.problem = show ( Inter.Types.problem var )
			, P.aufgabe = Inter.Types.aufgabe var
			, P.version = Inter.Types.version var
			}
		in Inter.Env.get_with env def

    motd <- logged "motd" $ Inter.Motd.contents

    -- gültigkeit prüfen, aufg.-instanz generieren / bzw. holen
    --
    -- das ist eine IO-Aktion 
    -- (wg. DB-zugriffen und evtl. Random in Generierung)
    -- im Reporter.Type gibts (mit Absicht) (leider) keine IO
    res <- logged "validate" $ validate $ par0 { P.variants = variants }

    -- haben es wir geschafft zum aufgabenlösen
    case res of
     -- nein, fehler (=msg) : entweder passwort falsch 
     -- oder ungültige Aufgabe (-> zeige mögliche)
     Left msg -> do
          return $ page par0 { P.variants = variants }  ( msg +++ motd )

     -- ja,
     Right par1 -> case P.variante par1 of
       Variant v -> do

          -- key (für persönliche Aufgabe) und Aufgaben-Instanz
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

	  -- nur bei Submit wird textarea übernommen.
          ein <- case P.click par1 of
	      Submit   -> return $ P.input par1
	      Previous -> latest par1 
		  `Control.Exception.catch` \ _ -> return ( show b0 )
	      _  -> return $ show b0

          -- Eingabe.Form füllen entweder mit
          let par2 = par1 { P.input = ein }

	  ( res , ans, log ) <- 
	      if ( P.click par1 == Submit )
	      -- nur dann erfolgt berechnung und ausgabe der bewertung
	      then do  
		   -- neu (11. 11. 03): IO-Aktion ausführen
		   ( res :: Maybe Int , com :: Doc ) 
		       <- timed_run patience ( reject $ text "timer expired" ) 
		          $ do -- TODO: set default dir
			       evaluate ( problem v ) i par2
		   let ans = p << "Das Korrekturprogramm sagt:"
			 +++ p << pre << render com 
	            -- bewertung in datenbank und file
		   msg <- bank par2 res 
		   let log = p << "Eintrag ins Logfile:" +++ p << pre << msg
		   return ( res , ans , log )
	      else return ( Nothing , noHtml, noHtml )

	  -- Höhe der Eingabe-Form berechnen
          let height = length $ filter ( == '\n' ) $ P.input par2

	  -- bewertung ausgeben, bzw. zur Lösungseingabe auffordern
          let status = case res of
		   Just s -> 
			   p << bold << ( "Korrekte Lösung, Size: " ++ show s )
		   Nothing -> 
			   p << bold << ( "Hier Lösung eingeben:" )

			   +++ p << "(experimentell) Typ der Eingabe:"
			   +++ p << pre << show (toDoc $ typeOf b0)

			   +++ textarea ( primHtml $ P.input par2  ) 
					! [ name "Input"
					  , rows $ show $ height + 2
					  , cols $ show $ P.input_width par2
					  ]
			   +++ br

			   +++ submit "Click" ( show Submit   ) +++ " " 
			   +++ submit "Click" ( show Previous ) +++ " " 
			   +++ submit "Click" ( show Example  ) +++ " " 

			   +++ reset  "Click" ( show Reset    ) 

                           +++ hidden "Problem" ( P.problem par2 ) 
                           +++ hidden "Aufgabe" ( P.aufgabe par2 ) 
                           +++ hidden "Version" ( P.version par2 ) 


          return $ page par2 
		 $   pres -- experimentell (makers)
		 +++ inst -- aufgabenstellung (immer)
		 +++ motd -- message vom tage (immer)
		 +++ log  -- logfile entry (nur bei submit)
		 +++ status -- antwort OK bzw. Textarea für neue lösg (immer)
		 +++ ans  -- korrektur-log (nur bei submit)

------------------------------------------------------------------------
--
-- Die HTML-Seite und ihre Bestandteile
--
page par msg = 
    let 
	heading = h2 << "Autotool CGI Inter.Face"
	pref = preface par 
	var = varselector par 
	chg = primHtml "  Achtung, verwirft Lösung!!"
    in  header << thetitle << "Inter.Face"
            +++ body ( form ( foldr1 (+++) 
			       [ heading , hr , pref , hr , msg ] )
				   ! [ Text.Html.action "Face.cgi"
				 , method "POST" ]
		 )
         
preface par = table << aboves 
     [ besides
	$ txtf' "10" "Matrikel" ( P.matrikel par )
   	++ pwdf "Passwort" ( P.passwort par )
   	++ varselector par 
	++ [ td << submit "Click" ( show Change ) ]
     ]
       
varselector par =
    let vars = map primHtml  $ "--" : map show ( P.variants par )
    in  [ td << "Aufgabe "  ,  td << menu "Wahl" vars ]

txtf name cont = 
    [ td << name , td << textfield name ! [ value cont ] ]

txtf' sz name cont = 
    [ td << name , td << textfield name ! [ value cont , Text.Html.size sz ] ]

pwdf name cont = 
    [ td << name , td << password name ! [ value $ show  cont , Text.Html.size "10" ] ]

