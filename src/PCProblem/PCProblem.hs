-- | Korrekturfunktion für PCP-Aufgaben

-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

module PCProblem.PCProblem (
     PCProblem (..)
    ,Folge (..)
    ,module PCP.Type
    ,validiere
    ,verifiziere
    ) where

import FiniteMap
import PCP.Type

import Challenger
import ToDoc
import Set
import System

import List (isPrefixOf)


data PCProblem = PCProblem deriving Show

-- Indexfolge
type Folge = [ Integer ]

-- data Folge = Folge [Int] deriving (Show, Read)
-- instance ToDoc (Folge) where
-- 	toDoc (Folge flg) = text "Loesung: " <+> toDoc flg


-- Erzeugen der Instanz fuer die Klasse Problem 
-- Problem: PCProblem
-- Instanz: PCP [(String,String)]
-- Beweis: [Int]

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

instance 
    Partial PCProblem ( PCP pcp ) folge where

    initial PCProblem ( PCP pcp ) = 
        []
    partial PCProblem ( PCP pcp ) lsg = do
        let (l,r) = lr pcp folge
        assert ( isPrefixOf l r || isPrefixOf r l )
	    $ text "Ist das eine Wort ein Präfix des anderen?"

instance  Step PCProblem ( PCP pcp ) Folge ( Select Int ) where
        step Vertex ( g, k ) xs ( Pick x ) =
            xs ++ [x]

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


lr :: PCP -> Folge -> ( String, String )
lr (PCP pcp) folge = 
    let links  = do k <- folge ; let { (l,r) = pcp !! fromIntegral (k-1) } ; l
	rechts = do k <- folge ; let { (l,r) = pcp !! fromIntegral (k-1) } ; r
    in	( links, rechts )

common :: Eq a => [a] -> [a] -> [a]
-- längster gemeinsamer prefix
common [] ys = []
common xs [] = []
common xxs @ (x : xs) yys @ (y : ys) =
    if x == y then x : common xs ys else []

---------------------------------------------------------------------------

-- erzeugt den Ausgabestring fuer die HTML Ausgabe der PCP-Instanz
erzInstanz :: PCP -> String
erzInstanz (PCP xys) = unlines $ do 
    (x, y) <- xys
    return $ "<tr><td>" ++ x ++ "</td><td>" ++ y ++ "</td></tr>"

	

-- erzeugt den AusgabeString fuer die HTML Ausgabe des Beweises 
erzBeweis :: PCP -> Folge -> String
erzBeweis pcp ks = 
    let (links, rechts) = lr pcp ks
    in	unlines [ "Lösungsfolge: " ++ show ks ++ "<BR>"
		, "expandierte Lösungsfolge: " ++ show links
		]



--Beispielstrukturen
bsp_pcp = PCP [("001","0"), ("10","011"), ("01","001") ]


bsp_folge =  [1,2,3,1]
