{-# OPTIONS -fallow-overlapping-instances #-}

module Machine.Fun

--   $Id$

( fun_test
, inner_fun_test
, numerical_test
, numerical_test'
)

where

import Machine.Class
import Machine.Vorrechnen
import Machine.Akzeptieren
import Machine.History

import qualified Machine.Numerical.Type as N

import Control.Monad (guard)
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Set
import Autolib.Size

numerical_test' ::  ( Numerical dat, Machine m dat conf, Out m dat conf )
	        => N.Type m
		-> m
		-> Reporter Int
numerical_test' i m = numerical_test ( N.cut i ) ( N.args i ) ( N.fun i ) m


numerical_test :: ( Numerical dat, Machine m dat conf, Out m dat conf )
	 => Int
	 -> [[Integer]] -- Liste von eingabe-vektoren
	 -> ( [Integer] -> Integer ) -- die funktion ist auszurechnen
	 -> m
	 -> Reporter Int
numerical_test cut inputs fun m = do
    let check ein conf = do

            r <- Machine.Class.output_reporter m conf

	    let a = decode r
	    let a' = fun ein

	    inform $ vcat
		   [ text "Die Endkonfiguration enthält das Resultat", toDoc a
		   , text "gefordert war", toDoc a'
		   ]
	    return $ a == a' 
    inner_fun_test cut inputs encode check m	    


fun_test :: ( Machine m dat conf , Out m dat conf )
     => Int 
     -> [(dat,dat)]  -- Liste von Paaren von Eingabe/Ausgabe
     -> m
     -> Reporter Int
fun_test cut pairs m = do
    let fm = listToFM pairs
    let check ein conf = do
	    aus <- Machine.Class.output_reporter m conf
            let Just wanted = lookupFM fm ein
		res = wanted == aus
	    inform $ vcat [ text "wird die geforderte Endkonfiguration"
			    <+> toDoc wanted <+> text "erreicht?"
			  , nest 4 $ toDoc res
			  ]
	    return res
    inner_fun_test cut ( map fst pairs ) id check m	    


inner_fun_test :: (ToDoc e, Machine m dat conf, Out m dat conf )
     => Int 
     -> [ e ] -- Liste von eingaben
     -> ( e -> dat ) -- input encoding
     -> ( e -> conf -> Reporter Bool ) -- ausgabe korrekt?
     -> m
     -> Reporter Int
inner_fun_test cut inputs encode check m = do
    inform $ text $ "Ihre Maschine ist"
    inform $ nest 4 $ toDoc m

    inform $ text "Bei allen folgenden Rechnungen berücksichtige ich"
    inform $ text $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

    -- inform $ text $ "ich starte die Maschine auf einigen Eingaben"
    -- vorrechnens m $ take 4 $ drop 2 $ map encode $ inputs

    richtige_ergebnisse cut m inputs encode check

    return $ size m


richtige_ergebnisse :: ( ToDoc e, Machine m dat conf, Out m dat conf )
	      => Int 
	      -> m 
	      -> [e] -- Liste von eingaben
	      -> ( e -> dat ) -- input encoding
	      -> ( e -> conf -> Reporter Bool ) -- ausgabe korrekt?
              -> Reporter ()
richtige_ergebnisse cut m inputs encode check = do
   mapM_ ( re cut m encode check ) inputs
   inform $ text "zu allen Eingaben wurde"
   inform $ text "die richtige Ausgabe berechnet"


re :: ( ToDoc e, Machine m dat conf, Out m dat conf )
	      => Int 
	      -> m 
	      -> ( e -> dat ) -- input encoding
	      -> ( e -> conf -> Reporter Bool ) -- ausgabe korrekt?
	      -> e
              -> Reporter ()

re cut m encode check ein = do
    let s = encode ein
    c <- input_reporter m s
    let aks = akzeptierend_oder_ohne_nachfolger cut m $ c
	ks = filter ( isEmptySet . next m ) aks
    when ( null ks )
	 $ reject $ vcat [ vcat [ text  "Bei Eingabe", toDoc ein 
				, text "Startkonfiguration", toDoc s
				]
			 , text "erreicht die Maschine keine Endkonfiguration."
			 , text "einige (erfolglose) Rechungen sind:"
			 , nest 4 $ vcat $ map present $ take 3 $ aks
		         ] 
    sequence_ $ do 
       k <- ks
       return $  do
           -- let b = Machine.Class.output m k
	   inform $  vcat [ text "Bei Eingabe", toDoc ein
			    , text "Startkonfiguration", nest 4 $ toDoc s
			    , text "erreicht die Maschine"
			    , text "die Endkonfiguration", nest 4 $ toDoc k
			    ]
           when ( not $ accepting m k ) $ do
	        reject $ text "Das ist keine akzeptierende (erfolgreiche) Konfiguration."
           f <- check ein k
	   when ( not f ) $ do
	           -- vorrechnen m s
	           reject $ present k

  
