-- | Korrekturfunktion für PCP-Aufgaben

-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

module PCProblem.Verify where

import PCProblem.Type

import Challenger
import Reporter

import ToDoc
import Set
import System

import List (isPrefixOf)


instance (ToDoc PCP, Show PCP, Read PCP
         , ToDoc Folge, Show Folge, Read Folge)
    => Problem PCProblem PCP Folge where

    validiere PCProblem (PCP  pcp) folge = 
        let range = mkSet [ 1 .. fromIntegral $ length pcp ]
	    aussen = filter ( \ i -> not (elementOf i range) ) folge
	in  if null folge 
	    then ( False, text "Die Lösungsfolge darf nicht leer sein." )
	    else if not $ null aussen 
	    then ( False, text "Diese Elemente der Lösungsfolge bezeichnen kein Paar der Instanz:" <+> toDoc aussen )
	    else ( True, text "OK" )
        
    verifiziere PCProblem p folge = 
        let ( links, rechts ) = lr p folge
	    c = common links rechts
	    linksrest = drop (length c) links
	    rechtsrest = drop (length c) rechts
	in  if links == rechts
	    then ( True, text "Oberes und unteres Wort stimmen überein:"
			 <+> toDoc links )
	    else ( False
		 , fsep [ text "Der längste gemeinsame Präfix des oberen und unteren Wortes ist" <+> toDoc c
			, text "Der Rest des oberen Wortes ist:" <+> toDoc linksrest
			, text "Der Rest des unteren Wortes ist:" <+> toDoc rechtsrest
			]
		 )


-----------------------------------------------------------------------

    -- Erzeugt HTML-File zur Visualisierung
    getInstanz PCProblem pcp folge dateiName =
	 do 
	  writeFile (dateiName ++ ".html") ("<br><table borders><caption>PCP-Instanz</caption>" ++ (erzInstanz pcp) ++ "</table>")
	  return (dateiName ++ ".html","html",ExitSuccess)
        
    -- Erzeugt HTML-File zur Visualisierung
    getBeweis PCProblem pcp folge dateiName =
	 do 
	  writeFile (dateiName ++ ".html") (erzBeweis pcp folge)
	  return (dateiName ++ ".html","html",ExitSuccess)



