module Turing.Laufzeit 

( Laufzeit (..)
, test, wrapped_test -- classical
)

where

-- $Id$

import Turing
import Turing.Akzeptieren
import Turing.Vorrechnen
import Turing.Konfiguration

import Turing.Laufzeit.Type

import Challenger

import Informed
import Reporter
import qualified Reporter.Result
import ToDoc
import Size

wrapped_test note f args m = Reporter.Result.wrapper ( test note f args m )

test :: String
     -> (Int -> Int) -> [ Int ]
     -> Turing Char Integer 
     -> Reporter Int
test n f a m = do
    verifiziereR TM ( Laufzeit { fun = f , fun_info = text n , args = a } ) m
    return $ size m

instance Partial TM Laufzeit ( Turing Char Integer ) where
    initial p i =
      Turing 
	{ eingabealphabet = mkSet "A"
	, arbeitsalphabet = mkSet "#ABC"
	, leerzeichen = '#'
	, zustandsmenge = mkSet [ 1 .. 3 ]
	, startzustand = 1
	, endzustandsmenge = mkSet [ 3 ]
	, tafel = collect [ (('A', 1), ('B', 2, L)) 
			  , (('B', 2), ('C', 3, L)) 
			  ]
	}		
    total = verifiziereR

instance Problem TM Laufzeit ( Turing Char Integer ) where

  verifiziereR TM conf m = do

    inform $ fsep [ text "Ihre Turingmaschine"
		  , text "soll für Eingaben der Form A^n"
		  , text "genau", info conf, text "Schritte ausführen."
		  ]

    inform $ text "Ihre Turingmaschine ist"
    inform $ toDoc m

    check m
    deterministisch m

    inform $ text "ich teste die Laufzeit für die Eingabelängen" 
	     <+> toDoc ( args conf )

    let falsch = do 
	  x <- args conf
	  let t = fun conf x
	  let ein = take  x $ repeat 'A' -- fixiertes eingabe-alphabet
	  let ks = akzeptierend (t+1) m ein
	  case ks of
		[] -> return 
		    ( ein
		    , fsep [ text "erreicht innerhalb der ersten"
			   , toDoc t, text "Schritte"
			   , text "keinen Endzustand." 
			   ]
		    )
		ks -> do k <- ks
			 guard $ nummer k /= t
			 return ( ein
				, fsep [ text "hält bereits im Schritt"
				       , toDoc (nummer k), text ","
				       , text "Laufzeit soll aber" 
				       , toDoc t, text "sein." 
				       ]
				)

    case  falsch of
        [] -> do 
		  inform $ text $ "alle Laufzeiten sind korrekt"
        wms -> do 
	    inform $ text  "diese Eingaben/Laufzeiten sind falsch:"
		   $$ ( vcat $ do 
			( ein, f ) <- wms
			return $ toDoc ein <+> f
		      )
	    vorrechnens m $ take 3 $ map fst falsch
            reject empty


