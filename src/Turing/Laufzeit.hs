module Turing.Laufzeit where

-- $Id$

import Turing
import Turing.Akzeptieren
import Turing.Vorrechnen
import Turing.Konfiguration

import Reporter

test :: TUM Char z
     => (Int -> Int) -> [ Int ]
     -> Turing Char z 
     -> Reporter Int

test f args m = do
    inform $ text "Ihre Turingmaschine ist"
    inform $ toDoc m

    check m
    deterministisch m

    inform $ text $ "ich teste die Laufzeit für die Eingabelängen" 
	     <+> toDoc args

    let falsch = do 
	  x <- args
	  let t = f x
	  let ein = take  x $ repeat 'A' -- fixiertes eingabe-alphabet
	  let ks = akzeptierend (t+1) m ein
	  case ks of
		[] -> return 
		    ( ein
		    , fsep [ text "erreicht innerhalb der ersten"
			   , toDoc t, text "Schritte"
			   , text "keinen Endzustand." 
		    )
		ks -> do k <- ks
			 guard $ nummer k /= t
			 return ( ein
				, fsep [ text "hält bereits im Schritt"
				       , toDoc (nummer k), text ","
				       , text "Laufzeit soll aber" 
				       , toDoc t, text "sein." 
				)

    case  falsch of
        [] -> do 
		  inform $ text $ "alle Laufzeiten sind korrekt"
        wms -> do 
	    inform $ vcat [ text $ "diese Eingaben/Laufzeiten sind falsch:"
			  , toDoc wms
			  ]
	    vorrechnens m $ take 3 $ map fst falsch
            reject empty


