
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

