module Robots.Type 

( module Robots.Data
, module Robots.Konfig 
, module Robots.Move
)

where

-- -- $Id$

import Robots.Data
import Robots.Konfig
import Robots.Move
import Robots.Nice

import FiniteMap
import ReadFM


import Challenger
import ToDoc

import Boc
import System 


-----------------------------------------------------------------------------

instance Problem Robots 
		 Konfig
		 [Zug]
	 where

    validiere   Robots k zs = 
        valid k

    verifiziere Robots k zs =
        executes k zs



    -- Erzeugt HTML-File zur Visualisierung
    getInstanz Robots rs zs dateiName =
	 do 
	  writeFile (dateiName ++ ".html") 
		    ("<br><table borders><caption>Roboter:</caption>" ++ erzInstanz rs ++ "</table>")
	  return (dateiName ++ ".html","html",ExitSuccess)
        
    -- Erzeugt HTML-File zur Visualisierung
    getBeweis  Robots rs zs dateiName =
	 do 
	  writeFile (dateiName ++ ".html") 
		    ("<br><table borders><caption>äquivalenter Ausdruck:</caption>" ++ (erzBeweis zs) ++ "</table>")
	  return (dateiName ++ ".html","html",ExitSuccess)

---------------------------------------------------------------------------

-- erzeugt den Ausgabestring fuer die HTML Ausgabe der Instanz
erzInstanz :: Konfig -> String
erzInstanz rs =
    "<tr><td><PRE>" ++ show (nice rs) ++ "</PRE></td></tr>"	

-- erzeugt den AusgabeString fuer die HTML Ausgabe des Beweises 
erzBeweis :: [Zug] -> String
erzBeweis zs = 
    "<tr><td>" ++ show zs ++ "</td></tr>"	






