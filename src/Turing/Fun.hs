module Turing.Fun

-- $Id$

( fun_test
)

where

import Turing
import Turing.Konfiguration

import Turing.Akzeptieren (akzeptierend)
import Turing.Vorrechnen


import Monad (guard)
import Reporter
import ToDoc
import Reader
import Size


fun_test :: (Reader [y], TUM y z)
     => Int -> [([y],[y])]  -- Liste von Paaren von Eingabe/Ausgabe
     -> Turing y z 
     -> Reporter Int

fun_test cut pairs m = do
    inform $ text $ "Ihre Turingmaschine ist"
    inform $ toDoc m

    inform $ text "Bei allen folgenden Rechnungen berücksichtige ich"
    inform $ text $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

    check m
    deterministisch m

    inform $ text $ "ich starte die Maschine auf einigen Eingaben"
    vorrechnens m $ take 4 $ drop 2 $ map fst pairs

    richtige_ergebnisse cut m pairs

    return $ size m


richtige_ergebnisse :: ( Reader [y] , TUM y z )
	      => Int -> Turing y z -> [([y],[y])] 
              -> Reporter ()
richtige_ergebnisse cut m  pairs = do
    let fehler = take 3 $ do 
          (ein, aus) <- pairs
          let ks = akzeptierend cut m ein
          if null ks 
	      then return (ein, reject $ fsep [ text  "Bei Eingabe", toDoc ein 
			     , text "erreicht die Maschine"
		             , text "keine Endkonfiguration." 
			     ] )
	      else do
	          k <- ks
	          let b = bandinhalt m k
	          guard $ b /= aus
	          return (ein, reject $ fsep [ text "Bei Eingabe", toDoc ein
			    , text "erreicht die Maschine"
			    , text "die Endkonfiguration", toDoc k
			    , text "gefordert ist aber der Bandinhalt"
			    , toDoc aus
			    ] )

    sequence_ $ map snd $ fehler
    if null fehler 
        then do
	     inform $ text "zu allen Eingaben wurde"
	     inform $ text "die richtige Ausgabe berechnet"
	else do
	     inform $ text "zu diesen Eingaben wurde"
             inform $ text "keine oder eine falsche Ausgabe berechnet:"
	     vorrechnens m $ map fst fehler


