
-- Korrekturfunktion für 3SAT
-- 
-- Autor: Mohammad Esad-Djou
-- bss98aou@studserv.uni-leipzig.de

-- $Id$

-- Gegeben: Aussagenlogik Formel F in konjunktive Normalform 
--         (mit genau 3 Konjunktionsgliedern)
-- Gesucht: Ist F erfüllbar?
-- Problem ist 3SAT, d.h. Erfüllbarkeitsproblem. 
--     => ein Datentyp mit einem konstanten Konstruktor
-- Instanz ist Formel F. 
--     => ein Datenstruktur
-- Beweis ist Belegung b, die Formel f erfüllt.
--     => ein Datenstruktur


module SAT.SAT 

( SAT (..)
, Variable, Literal (..)
, Klausel, Formel, Belegung
, module FiniteMap
, var
)

where

import FiniteMap
import ReadFM
import Challenger
import ToDoc
import Monad (guard)
import Sets
import System

import Step
import Interactive.Type
import qualified Component as C
import Reporter
import Maybe

-- ???
import Number
import Iso

import SAT.Types
import SAT.Inter

variablen :: Formel -> Set Variable
variablen f = mkSet $ do
    ( l1, l2, l3 ) <- f
    l <- [ l1, l2, l3 ]
    return $ case l of Pos v -> v ; Neg v -> v


instance Problem SAT Formel Belegung where 

 
   validiere SAT f b = 
       let vf = variablen f
	   vb = mkSet (keysFM b)
           fb = minusSet vf vb
	   bf = minusSet vb vf
       in
	if not ( isEmptySet fb )
	then ( False, text "Diese Variablen sind nicht belegt:" <+> toDoc fb )
	else if not ( isEmptySet bf )
	then ( False, text "Diese Variablen sind nicht in der Formel:" <+> toDoc bf )
	else (True, text "Die Belegung passt zur Formel.")


   verifiziereR SAT f b = do
        partial SAT  f b
	total   SAT f b


-- Erzeugt HTML-File zur Visualisierung
   getInstanz SAT f b dateiName =
	do 
	 writeFile (dateiName ++ ".html")
	            ("<br><table borders><caption>Diese Formael ist zu erfüllen:</caption>" ++ (erzInstanz f) ++ "</table>")
	 return (dateiName ++ ".html", "html",ExitSuccess)


-- Erzeugt HTML-File zur Visualisierung
   getBeweis SAT f b dateiName =
	do
	 writeFile (dateiName ++ ".html")
		    (erzBeweis b)
	 return (dateiName ++ ".html", "html",ExitSuccess)


instance Number Formel Formel where number = id 

-- Version 1
instance Iso Formel where iso f1 f2 = (mkSet f1) == (mkSet f2)


---------------------------------------------------------------------------

instance Partial SAT Formel Belegung where


    initial SAT g = emptyFM

    partial SAT f b = do

        let domain = mkSet $ keysFM b
	    out = minusSet domain ( variablen f )
        when ( not $ isEmptySet out ) $ reject $ vcat
	     [ text "Diese Variablen der Belegung"
	     , text "gehören gar nicht zur Formel:"
	     , nest 4 $ toDoc out
	     ]

	let wrong = do 
	        klaus <- f
		False <- maybeToList $ m_wert_klausel klaus b
		return klaus
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese vollständig belegten Klauseln sind nicht erfüllt:"
	     , nest 4 $ toDoc wrong
	     ]
	inform $ text "Alle vollständig belegten Klauseln sind erfüllt."
	       

    total SAT f b = do
        let fehl = minusSet ( variablen f ) ( mkSet $ keysFM b )
	when ( not $ isEmptySet fehl ) $ reject $ vcat
	     [ text "Diese Variablen sind nicht belegt:"
	     , nest 4 $ toDoc fehl
	     ]
	inform $ text "Alle Variablen sind belegt."



instance  Step SAT Formel Belegung ( Paint String (Maybe Bool) ) where
        step SAT f b ( Paint v mc ) =
	    case mc of
	         Nothing -> delFromFM b v
		 Just c  -> addToFM   b v c

instance  Interactive SAT Formel Belegung ( Paint String (Maybe Bool) ) where

  interactive SAT f  = do

    let doc = ( C.mkLabel "rechte Maustaste ergibt Popup-Menu" )
	      { C.ident = C.Ident "doc" }

    let handle v = \ ( C.Input cs) -> Paint v $
           case reads cs of
	        [(mc, "")] -> mc
		_          -> Nothing

    let result listener = 
          let vccs = do 
		 v <- setToList $ variablen f
		 let c0 = ( C.mkLabel v ) 
			  { C.ident = C.Ident $ v ++ "0" 
			  }
		 let c1 = ( C.mkChoice $ map show 
			               $ Nothing : map Just [ False, True ] ) 
			  { C.ident = C.Ident $ v ++ "1"
			  , C.action = C.translate ( handle v ) listener
			  }
		 return (v, c0, c1)

              set = C.splits $ do
			(v, c0, c1) <- vccs
			return $ C.translate ( \ f -> 
				      [ C.Text $ show $ lookupFM f v ] )
			       $ C.changeL c1 
          in ( C.column [ doc
			, C.grid $ do 
			     ( v, c0, c1 ) <- vccs
			     return [ c0, c1 ]
			]
	     , set )


    return $ result




----------------------------------------------------------------------------


-- erzeugt den Ausgabestring fuer die HTML Ausgabe der Instanz
erzInstanz :: Formel -> String
erzInstanz f = "<tr><td>" ++ show f ++ "</td></tr>"

-- erzeugt den Ausgabestring fuer die HTML Ausgabe des Beweises
erzBeweis :: Belegung -> String
erzBeweis b = "<tr><td>" ++ show b ++ "</td></tr>"


wert_formel :: Formel -> Belegung -> Bool
wert_formel f b =
    and [ wert_klausel k b | k <- f ]

wert_klausel 	:: Klausel -> Belegung -> Bool
wert_klausel (l1,l2,l3) b = wert_literal l1 b || wert_literal l2 b || wert_literal l3 b

wert_literal 	:: Literal -> Belegung -> Bool
wert_literal  (Pos v) b = wert_variable v b
wert_literal  (Neg v) b = not (wert_variable v b)
			
wert_variable 	:: Variable -> Belegung -> Bool
wert_variable v b =
    case lookupFM b v of
	Nothing -> error "variable nicht in belegung."
	Just w  -> w

-----------------------------------------------------------

m_wert_formel :: Formel -> Belegung -> Maybe Bool
m_wert_formel f b = do
    ws <- mapM (flip m_wert_klausel b) f
    return $ and ws

m_wert_klausel :: Klausel -> Belegung -> Maybe Bool
m_wert_klausel (l1, l2, l3) b = do
    ws <- mapM (flip m_wert_literal b) [l1, l2, l3]
    return $ or ws

m_wert_literal 	:: Literal -> Belegung -> Maybe Bool
m_wert_literal (Pos v) b = m_wert_variable v b
m_wert_literal (Neg v) b = fmap not $ m_wert_variable v b

m_wert_variable 	:: Variable -> Belegung -> Maybe Bool
m_wert_variable v b = lookupFM b v
