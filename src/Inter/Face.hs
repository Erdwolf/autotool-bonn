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


import Network.CGI
import Text.Html hiding ( text  )

import Autolib.ToDoc (Doc, render, toDoc, text)

import Autolib.Reporter
import qualified Autolib.Output
import qualified Autolib.Output.Html


import Inter.Env
import Inter.Validate
import Inter.Evaluate

import Inter.Bank
import Inter.Store ( latest )
import Inter.Click

import Control.Types ( fromCGI, ToString (..) )
import qualified Inter.Param as P
import qualified Inter.Motd

import qualified Challenger

import Inter.Types
import Inter.Make
import qualified Control.Exception


import Data.Typeable


import Autolib.Timer
import Inter.Logged

-- hier sind die aufgaben drin:
import qualified Inter.Collector

import Autolib.Informed

patience :: Int
patience = 15 -- seconds

main :: IO ()
main = wrapper $ \ env ->  
         iface Inter.Collector.makers env
             `Control.Exception.catch` \ err -> 
                 logged ( "catching" ++ show err )
			$ return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

iface :: [ Make ] -> [(String, String)] -> IO Html
iface makers env = logged "iface" $ do

    -- alle Inputs aus Env. holen
    let par0 = Inter.Env.get_with env $
	       P.empty { P.makers = makers }
    motd <- logged "motd" $ Inter.Motd.contents

    -- gültigkeit prüfen, aufg.-instanz generieren / bzw. holen
    --
    -- das ist eine IO-Aktion 
    -- (wg. DB-zugriffen und evtl. Random in Generierung)
    -- im Reporter.Type gibts (mit Absicht) (leider) keine IO
    res <- logged "validate" $ validate $ par0 

    -- haben es wir geschafft zum aufgabenlösen
    case res of
     -- nein, fehler (=msg) : entweder passwort falsch 
     -- oder ungültige Aufgabe (-> zeige mögliche)
     Left ( msg, par1 ) -> logged ( "lefty" ++ show msg ) $ do
          return $ page par1 ( msg +++ motd )


     -- ja,
     Right par1 -> case P.variante par1 of
       Variant v -> do

          -- key (für persönliche Aufgabe) und Aufgaben-Instanz
          -- herstellen und anzeigen
          k <- key  v $ P.smatrikel par1
          generator <- gen v k
	  -- TODO: das com sollte eigentlich leer sein
	  -- (Beschreibung ist jetzt in report/describe)
          let ( Just i, com :: Doc ) = export generator

          ( _ , desc :: Html ) <- run $ do
		  -- FIXME: set default dir
	          Challenger.report (problem v) i
          let inst =  h3 << "Aufgabenstellung"
		   +++ p  <<  desc
		   +++ h3 << "Hinweise"
                   +++ p << toString (P.remark par1)

          -- Beispiel-Eingabe holen  
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

                           +++ hidden "Aufgabe" ( P.saufgabe par2 ) 


          return $ page par2 
		 $   inst -- aufgabenstellung (immer)
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
	heading = h2 << "Autotool-CGI-Inter.Face"
	pref = preface par 
	chg = primHtml "  Achtung, verwirft Lösung!!"
    in  header << thetitle << "Inter.Face"
            +++ body ( form ( foldr1 (+++) 
			       [ heading , hr , pref , hr , msg ] )
				   ! [ Text.Html.action "Face.cgi"
				 , method "POST" ]
		 )
         
preface par = table << aboves 
     [ besides	$ 
           txtf' "10" "Matrikel" ( toString $ P.mmatrikel par )
   	++ pwdf "Passwort" ( toString $ P.mpasswort par )
  	++ varselector ( P.names par )
	++ [ td << submit "Click" ( show Change ) ]
     ]


instance ToString a => ToString (Maybe a) where   
    toString mx = case mx of Just x -> toString x ; Nothing -> ""

varselector nms =
    let vars = map ( primHtml . toString ) nms
    in  if null nms 
	then [ ] 
	else [ td << "Aufgabe "  ,  td << menu "Aufgabe" vars ]

txtf name cont = 
    [ td << name , td << textfield name ! [ value cont ] ]

txtf' sz name cont = 
    [ td << name , td << textfield name ! [ value cont , Text.Html.size sz ] ]

pwdf name cont = 
    [ td << name , td << password name ! [ value cont , Text.Html.size "10" ] ]

