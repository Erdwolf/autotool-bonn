-- | Korrekturfunktion für PCP-Aufgaben
--
-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

module PCProblem.PCProblem (
     PCProblem
    ,Folge (..)
    ,module PCP.Type
    ,validiere
    ,verifiziere
    ) where

import FiniteMap
import PCP.Type
--import PCP.Util
import Challenger
import ToDoc
import Set
import System


data PCProblem = PCProblem deriving Show
data Folge = Folge [Int] deriving (Show, Read)

instance ToDoc (Folge) where
	toDoc (Folge flg) = text "Loesung: " <+> toDoc flg


-- Erzeugen der Instanz fuer die Klasse Problem 
-- Problem: PCProblem
-- Instanz: PCP [(a,(String,String))]
-- Beweis: Folge 

instance (ToDoc PCP, Show PCP, Read PCP
         , ToDoc Folge, Show Folge, Read Folge)
    => Problem PCProblem PCP Folge where

    validiere PCProblem ((PCP pcp)) (folge @ (Folge fol)) = 
    		if and [(not(pcp == [])),not(fol == []),folgeIsInPcP (listToFM pcp) folge]
			    then (True, text "PCP Ok.")
			    else (False, text "PCP ist nichtkorrekt")
        
    verifiziere PCProblem (PCP pcp) lsg = (pcpTest (listToFM pcp) lsg "" "")

    -- Erzeugt HTML-File zur Visualisierung
    getInstanz PCProblem (PCP pcp) folge dateiName =
	 do 
	  writeFile (dateiName ++ ".html") ("<br><table borders><caption>PCP-Instanz</caption>" ++ (erzInstanz pcp) ++ "</table>")
	  return (dateiName ++ ".html","html",ExitSuccess)
        
    -- Erzeugt HTML-File zur Visualisierung
    getBeweis PCProblem (PCP pcp) (fol @ (Folge folge)) dateiName =
	 do 
	  writeFile (dateiName ++ ".html") ("L&ouml;sungsfolge: " ++ show folge ++ "\n<br>" ++ (erzBeweis (listToFM pcp) fol ""))
	  return (dateiName ++ ".html","html",ExitSuccess)



-- erzeugt den Ausgabestring fuer die HTML Ausgabe der PCP-Instanz
erzInstanz :: [(Int,(String,String))] -> String
erzInstanz (x:xs) = 
	let
	(a,(str1,str2)) = x
	in "<tr><td>" ++ str1 ++ "</td><td>" ++ show a ++ "</td><td>" ++ str2 ++ "</td></tr>\n" ++ (erzInstanz xs)
erzInstanz [] = ""    
	

-- erzeugt den AusgabeString fuer die HTML Ausgabe des Beweises 
erzBeweis :: FiniteMap Int (String,String) -> Folge -> String -> String
erzBeweis pcp (Folge []) str1 = "L&ouml;sungsketten: " ++ str1 ++ "<br>\n"
erzBeweis pcp (Folge (x:xs)) str1 =
	let
	(add1,add2) = (lookupWithDefaultFM pcp (error "PCProblems.erzBeweis") x)
	in
	    erzBeweis pcp (Folge xs) (str1 ++ add1)



--Funktion erzeugt die beiden Ketten und vergleicht sie
pcpTest :: FiniteMap Int (String,String) -> Folge -> String -> String -> (Bool,Doc) 
pcpTest pcp (Folge []) str1 str2 = if str1 == str2
									then (True,text "Korrektes PCP\n" <+> text "Kette 1/2:" <+> text str1)
                                    else (False,text "Ketten sind nicht identisch" <+> 
                                    	text "\nKette1: " <+> text str1 <+> text "\nKette2: " <+> text str2)
pcpTest pcp (Folge (x:xs)) str1 str2 = 
	let
    	(add1,add2) = (lookupWithDefaultFM pcp (error "PCProblems.pcpTest") x)	   
    in    
		pcpTest pcp (Folge xs) (str1 ++ add1) (str2 ++ add2)



--Testet ob Folgenelemente im PCP sind
folgeIsInPcP :: FiniteMap Int (String,String) -> Folge -> Bool
folgeIsInPcP pcp (Folge []) = True
folgeIsInPcP pcp (Folge (x:xs)) = if (elemFM x pcp) then (folgeIsInPcP pcp (Folge xs)) else False




--Beispielstrukturen
bsp_pcp =PCP([(1,("001","0")),
        	(2,("10","011")),
                 (3,("01","001"))
                ])


bsp_folge = Folge [1,2,3,1]
