-- | Korrekturfunktion für PCP-Aufgaben
--
-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

module PCProblem.PCProblem (
	  PCProblem
         ,Folge (..)
	 ,module PCP.Type
	 ,validiere
	 ,verifiziere) 
  	where

import FiniteMap
import PCP.Type
--import PCP.Util
import Challenger
import ToDoc
import Set


data PCProblem = PCProblem deriving Show
data Folge a = Folge [a] deriving (Show, Read)

instance (ToDoc [a]) => ToDoc (Folge a) where
	toDoc (Folge flg) = text "Loesung: " <+> toDoc flg


instance (ToDoc (PCP a), Show (PCP a), Read (PCP a), Iso (PCP a)
         , ToDoc (Folge a), Show (Folge a), Read (Folge a), Ord a, ToDoc a)
    => Problem PCProblem (PCP a) (Folge a) where

    validiere PCProblem ((PCP pcp)) (folge @ (Folge fol)) = 
    		if and [(not(pcp == [])),not(fol == []),folgeIsInPcP (listToFM pcp) folge]
			    then (True, text "PCP Ok.")
			    else (False, text "PCP ist nichtkorrekt")
        
    verifiziere PCProblem (PCP pcp) lsg = (pcpTest (listToFM pcp) lsg "" "")




--Funktion erzeugt die beiden Ketten und vergleicht sie
pcpTest :: (Ord a) => FiniteMap a (String,String) -> Folge a -> String -> String -> (Bool,Doc) 
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
folgeIsInPcP :: Ord a => FiniteMap a (String,String) -> Folge a -> Bool
folgeIsInPcP pcp (Folge []) = True
folgeIsInPcP pcp (Folge (x:xs)) = if (elemFM x pcp) then (folgeIsInPcP pcp (Folge xs)) else False




--Beispielstrukturen
bsp_pcp =PCP([('A',("001","0")),
        		 ('B',("10","011")),
                 ('C',("01","001"))
                ])


bsp_folge = Folge ['A','B','C','A']
