module Main where

-- $Id$

-- einfaches interface, keine komplexe zustandsfolge wie bei wash,
-- sondern jedesmal kompletter neustart.

import Network.CGI
import Text.Html hiding ( size, text )
import ToDoc (Doc, render)

import Reporter
import qualified Output
import qualified Output.Html

import Inter.Env
import Inter.Validate
import Inter.Evaluate
import Inter.Bank
import qualified Inter.Param as P
import qualified Challenger

import Inter.Types
import qualified Exception

import Inter.Boiler

main :: IO ()
main = do
     vs <- boiler
     wrapper $ \ env -> 
	 iface vs env
	 `Exception.catch` \ err -> return $ p << pre << primHtml ( show err )

------------------------------------------------------------------------

iface :: [ Variant ] -> [(String, String)] -> IO Html
iface variants env = do

    -- alle inputs holen
    let par0 = Inter.Env.get env

    -- gültigkeit prüfen, aufg.-instanz generieren
    -- das ist eine IO-Aktion 
    -- (wg. DB-zugriffen und evtl. Random in Generierung)
    -- im Reporter.Type gibts (mit Absicht) (leider) keine IO
    res <- validate $ par0 { P.variants = variants }

    case res of
	 Left msg -> do
	      return $ page par0 msg
	 Right par1 -> case P.variante par1 of
	   Variant v -> do
	      -- key/instanz herstellen und anzeigen
	      k <- key  v $ P.matrikel par1
	      generator <- gen v k
	      let ( Just i, com :: Doc ) = export generator
	      let inst = p << "Die Aufgabenstellung ist:"
			 +++ p << pre << render com

              let b0 = Challenger.initial ( problem v ) i
	      let par2 = par1 { P.input = if null ( P.input par1 )
				        then show b0 else P.input par1
			      }

	      -- eingabe bewerten ( echter Reporter )
	      let ( res :: Maybe Int , com :: Doc ) 
		      = export $ evaluate ( problem v ) i par2
	      let ans = p << "Das Korrekturprogramm sagt:"
			+++ p << pre << render com 

	      -- bewertung in datenbank und file
	      msg <- bank par2 res
	      let log = p << "Eintrag ins Logfile:"
			+++ p << pre << msg

	      let height = length $ filter ( == '\n' ) $ P.input par2
	      let status = case res of
		     Just s -> 
			 p << ( "Korrekte Lösung, Size: " ++ show s )
		     Nothing -> 
			 p << "Lösung ist nicht korrekt. Nochmal:"
			 +++ textarea ( primHtml $ P.input par2  ) 
			     ! [ name "input"
			       , rows $ show $ height + 2
			       , cols $ show $ P.input_width par2
			       ]

	      return $ page par2 $ inst +++ log +++ status +++ ans

------------------------------------------------------------------------

page par msg = 
    let pre = preface par
	sub = submit "submit" "submit"
    in  header << thetitle << "Inter.Face"
             +++ body ( form ( pre +++ sub +++ msg )
		      ! [ Text.Html.action "Face.cgi"
			, method "POST" ]
		      )
	     
preface par = table << 
    aboves [ besides $  txtf "matrikel" ( P.matrikel par )
		     ++ pwdf "passwort" ( P.passwort par )
		     
	    , besides $  txtf "problem" ( P.problem par )
		     ++ txtf "aufgabe" ( P.aufgabe par )
		     ++ txtf "version" ( P.version par )
	    ]

txtf name cont = 
    [ td << name , td << textfield name ! [ value cont ] ]
pwdf name cont = 
    [ td << name , td << password name ! [ value $ show  cont ] ]
