module Machine.Fun

-- $Id$

( fun_test
)

where

import Machine.Class
import Machine.Vorrechnen
import Machine.Akzeptieren


import Monad (guard)
import Reporter
import ToDoc
import Reader
import Size


fun_test :: Machine m dat conf 
     => Int -> [(dat,dat)]  -- Liste von Paaren von Eingabe/Ausgabe
     -> m
     -> Reporter Int

fun_test cut pairs m = do
    inform $ text $ "Ihre Maschine ist"
    inform $ toDoc m

    inform $ text "Bei allen folgenden Rechnungen berücksichtige ich"
    inform $ text $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

--    check m
--    deterministisch m

    inform $ text $ "ich starte die Maschine auf einigen Eingaben"
    vorrechnens m $ take 4 $ drop 2 $ map fst pairs

    richtige_ergebnisse cut m pairs

    return $ size m


richtige_ergebnisse :: Machine m dat conf
	      => Int -> m -> [(dat,dat)] 
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
	          let b = Machine.Class.output m k
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

  
