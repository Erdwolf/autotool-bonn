module Turing.Fun

-- $Id$

( fun_test
)

where

import Turing
import Turing.Konfiguration

import Turing.Akzeptieren (akzeptierend)
import Turing.Vorrechnen

import Auswertung
import Right
import Monad (guard)
import Random


fun_test :: TUM y z
     => Int -> [([y],[y])]  -- Liste von Paaren von Eingabe/Ausgabe
     -> Turing y z 
     -> IO String

fun_test cut pairs m = do
    putStrLn $ "Ihre Turingmaschine ist"
    putStrLn $ show m

    putStrLn $ "Bei allen folgenden Rechnungen berücksichtige ich"
    putStrLn $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

    muss (check m) $ do

      putStrLn $ "ich starte die Maschine auf einigen Eingaben\n"
      ws <- sequence $ take 4 $ repeat $ do 
	    i <- randomRIO ( 0 , length pairs - 1 )
	    return $ fst $ pairs !! i
      vorrechnens m ws

      muss (richtige_ergebnisse cut m pairs) $

	  right


bandinhalt :: TUM y z 
	   => Turing y z -> Konfiguration y z -> [ y ]
bandinhalt m k = 
    let e = leerzeichen m
        strip_links  = dropWhile (== e) 
	strip_rechts = reverse . strip_links . reverse
	band = reverse ( band_links  k ) ++ aktuelles_zeichen k : band_rechts k
    in	strip_links . strip_rechts $ band

richtige_ergebnisse :: TUM y z
	      => Int -> Turing y z -> [([y],[y])] -> Either String String
richtige_ergebnisse cut m  pairs = case take 3 $
    do (ein, aus) <- pairs
       let ks = akzeptierend cut m ein
       if null ks 
	  then return ( ein
		      ,  "Bei Eingabe " ++ show ein ++ " erreicht die Maschine"
		      ++ " keine Endkonfiguration." 
		      )
	  else do
	      k <- ks
	      let b = bandinhalt m k
	      guard $ b /= aus
	      return ( ein
		     , "Bei Eingabe " ++ show ein ++ " erreicht die Maschine"
		     ++ " die Endkonfiguration " ++ show k ++ ",\n"
		     ++ " gefordert ist aber der Bandinhalt " ++ show aus
		     )
  of []  -> Right $ "zu allen Eingaben wurde die richtige Ausgabe berechnet"
     ems -> Left  $ unlines 
		  $ ( "zu diesen Eingaben wurde keine oder eine falsche Ausgabe berechnet:"
		    : do { (ein, msg) <- ems; return msg }
		    ++ concat ( map (vorrechnen m) ( map fst ems ) )
		    )

  

