module Machine.Fun

-- $Id$

( fun_test
, numerical_test
)

where

import Machine.Class
import Machine.Vorrechnen
import Machine.Akzeptieren


import Monad (guard)
import Reporter hiding ( output )
import ToDoc
import Reader
import FiniteMap
import Size

numerical_test :: ( Numerical dat, Machine m dat conf )
	 => Int
	 -> [[Integer]] -- Liste von eingabe-vektoren
	 -> ( [Integer] -> Integer ) -- die funktion ist auszurechnen
	 -> m
	 -> Reporter Int
numerical_test cut inputs fun m = do
    let check ein aus = do
	    let a = decode aus
	    let a' = fun ein
	    inform $ fsep
		   [ text "Die Endkonfiguration enthält das Resultat", toDoc a
		   , text "gefordert war", toDoc a'
		   ]
	    return $ a == a' 
    inner_fun_test cut inputs encode check m	    


fun_test :: Machine m dat conf 
     => Int 
     -> [(dat,dat)]  -- Liste von Paaren von Eingabe/Ausgabe
     -> m
     -> Reporter Int
fun_test cut pairs m = do
    let fm = listToFM pairs
    let check ein aus = do
	    inform $ text "wird die geforderte Endkonfiguration"
		     <+> toDoc aus <+> text "erreicht?"
	    return $ lookupFM fm ein == Just aus
    inner_fun_test cut ( map fst pairs ) id check m	    


inner_fun_test :: (ToDoc e, Machine m dat conf )
     => Int 
     -> [ e ] -- Liste von eingaben
     -> ( e -> dat ) -- input encoding
     -> ( e -> dat -> Reporter Bool ) -- ausgabe korrekt?
     -> m
     -> Reporter Int
inner_fun_test cut inputs encode check m = do
    inform $ text $ "Ihre Maschine ist"
    inform $ toDoc m

    inform $ text "Bei allen folgenden Rechnungen berücksichtige ich"
    inform $ text $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

--    check m
--    deterministisch m

    inform $ text $ "ich starte die Maschine auf einigen Eingaben"
    vorrechnens m $ take 4 $ drop 2 $ map encode $ inputs

    richtige_ergebnisse cut m inputs encode check

    return $ size m


richtige_ergebnisse :: ( ToDoc e, Machine m dat conf )
	      => Int 
	      -> m 
	      -> [e] -- Liste von eingaben
	      -> ( e -> dat ) -- input encoding
	      -> ( e -> dat -> Reporter Bool ) -- ausgabe korrekt?
              -> Reporter ()
richtige_ergebnisse cut m inputs encode check = do
   mapM_ ( re cut m encode check ) inputs
   inform $ text "zu allen Eingaben wurde"
   inform $ text "die richtige Ausgabe berechnet"


re :: ( ToDoc e, Machine m dat conf )
	      => Int 
	      -> m 
	      -> ( e -> dat ) -- input encoding
	      -> ( e -> dat -> Reporter Bool ) -- ausgabe korrekt?
	      -> e
              -> Reporter ()

re cut m encode check ein = do
    let s = encode ein
    let ks = akzeptierend cut m $ s
    when ( null ks )
	 $ reject $ fsep [ text  "Bei Eingabe", toDoc ein 
			     , text "Startkonfiguration", toDoc s
			     , text "erreicht die Maschine"
		             , text "keine Endkonfiguration."
			     ] 
    sequence_ $ do 
       k <- ks
       return $  do
           let b = Machine.Class.output m k
	   inform $  fsep [ text "Bei Eingabe", toDoc ein
			    , text "Startkonfiguration", toDoc s
			    , text "erreicht die Maschine"
			    , text "die Endkonfiguration", toDoc k
			    ]
	   f <- check ein b
	   when ( not f ) $ do
	     inform $ text "so beginnt die fehlerhafte Rechnung:"
	     vorrechnen m s
	     reject $ empty

  
