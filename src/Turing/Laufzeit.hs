module Turing.Laufzeit where

-- $Id$

import Turing
import Turing.Akzeptieren
import Turing.Vorrechnen
import Turing.Konfiguration

import Reporter
import qualified Reporter.Result
import ToDoc
import Size

wrapped_test note f args m = Reporter.Result.wrapper ( test note f args m )

test :: TUM Char z
     => String
     -> (Int -> Int) -> [ Int ]
     -> Turing Char z 
     -> Reporter Int

test note f args m = do

    inform $ fsep [ text "Ihre Turingmaschine"
		  , text "soll für Eingaben der Form A^n"
		  , text "genau", text note, text "Schritte ausführen."
		  ]
    

    inform $ text "Ihre Turingmaschine ist"
    inform $ toDoc m

    check m
    deterministisch m

    inform $ text "ich teste die Laufzeit für die Eingabelängen" 
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
		  return $ size m
        wms -> do 
	    inform $ text  "diese Eingaben/Laufzeiten sind falsch:"
		   $$ ( vcat $ do 
			( ein, f ) <- wms
			return $ toDoc ein <+> f
		      )
	    vorrechnens m $ take 3 $ map fst falsch
            reject empty


