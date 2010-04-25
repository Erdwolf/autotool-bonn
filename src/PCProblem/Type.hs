{-# language TypeSynonymInstances, DeriveDataTypeable #-}

-- | Korrekturfunktion für PCP-Aufgaben

-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de



module PCProblem.Type 

( module PCProblem.Type
, module PCProblem.Data
)

where

import PCProblem.Data
import Autolib.Size
import Data.Typeable

{-
import Iso
import Number

-- Isomorphie Instanz
-- !!!!!!!!!!!!Muss noch fertig gestellt werden
instance Iso (PCP) where
	iso pcp1 pcp2 = False


instance Number PCP PCP where
   number = id
-}

data PCProblem = PCProblem deriving ( Show, Read, Typeable )

type Folge = [ Integer ]
instance Size Folge where size = length

---------------------------------------------------------------------------

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
