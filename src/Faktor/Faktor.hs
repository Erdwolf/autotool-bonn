-- | Korrekturfunktion für Faktorisierung
--
-- joe@informatik.uni-leipzig.de
-- benutzt code für challenger/PCProblem
-- von Markus Kreuz  mai99byv@studserv.uni-leipzig.de

module Faktor.Faktor (
     Faktor
    ) where


import Challenger
import ToDoc
import Number
import Iso

import System


data Faktor = Faktor deriving Show

instance Number Integer Integer where number = id



instance Problem Faktor Integer ( Integer, Integer ) where

    validiere Faktor x (y, z) = 
	   if x < 2
	   then ( False, text "Die Zahl soll größer als 1 sein." )
	   else if y == 1 || z == 1
	   then ( False, text "Kein Faktor darf gleich 1 sein." )
	   else ( True, text "OK" )

    verifiziere Faktor x (y, z) = 
        let yz = y * z  
        in if x /= yz
	   then ( False, fsep [ text "Das Produkt der beiden Zahlen"
			      , toDoc y, text "und", toDoc z
			      , text "ist" , toDoc yz
			      , text "und nicht", toDoc x
			      ] )				
	   else ( True, text "Das ist eine korrekte Faktorisierung" )

    -- Erzeugt HTML-File zur Visualisierung
    getInstanz Faktor x (y, z) dateiName =
	 do 
	  writeFile (dateiName ++ ".html") 
		    ("<br><table borders><caption>Diese Zahl ist zu faktorisieren:</caption>" ++ (erzInstanz x) ++ "</table>")
	  return (dateiName ++ ".html","html",ExitSuccess)
        
    -- Erzeugt HTML-File zur Visualisierung
    getBeweis Faktor x (y, z) dateiName =
	 do 
	  writeFile (dateiName ++ ".html") (erzBeweis (y, z))
	  return (dateiName ++ ".html","html",ExitSuccess)

---------------------------------------------------------------------------

-- erzeugt den Ausgabestring fuer die HTML Ausgabe der Instanz
erzInstanz :: Integer -> String
erzInstanz x = "<tr><td>" ++ show x ++ "</td></tr>"

	

-- erzeugt den AusgabeString fuer die HTML Ausgabe des Beweises 
erzBeweis :: (Integer, Integer) -> String
erzBeweis (y, z) = 
    "<tr><td>" ++ show y ++ "</td><td>" ++ show z ++ "</td></tr>"



