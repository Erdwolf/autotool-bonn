module Machine.Vorrechnen

where

-- -- $Id$

import Machine.Class
import Machine.History

import Set
import Schichten
import Reporter hiding ( output )
import ToDoc

vorrechnen :: Machine m dat conf
	   => m -> dat
	   -> Reporter ()
vorrechnen a xs = -- vorrechnen_cut 10 a xs
    vorrechnen_cut' 1000 10 a xs


vorrechnen_cut' :: Machine m dat conf
	   => Int -- max. soviele anzeigen
	   -> Int -- max. soviele erzeugen
	   -> m -> dat
	   -> Reporter ()
vorrechnen_cut' anz cut a xs = do
    inform $ fsep [ text "mit Eingabe", toDoc xs
		  , text "erreicht die Maschine unter anderem"
		  , text "folgende Konfigurationen:"
		  ]
    let ks = take cut $ nachfolger a $ input a xs
	stepping = length ks `div` anz
    let is = [ 1, anz + 1 .. length ks ]
    inform $ vcat $ do
        i <- is
	return $ present $ ks !! i


vorrechnen_cut :: Machine m dat conf
	   => Int 
	   -> m -> dat
	   -> Reporter ()
vorrechnen_cut cut a xs = do
    inform $  fsep [ text "mit Eingabe", toDoc xs
		  , text "erreicht die Maschine unter anderem"
		  , text "folgende Konfigurationen:"
                  ]
    inform $ nest 4 $ vcat $ do
        (i, ks) <- zip [0 :: Int .. ] 
		$ take cut
		$ map setToList 
		$ schichten ( next a )
		$ input a xs
	return  $ vcat $ do k <- ks; return $ toDoc k 

    
vorrechnens :: Machine m dat conf
	   => m -> [ dat ]
	   -> Reporter ()
vorrechnens a = mapM_ (vorrechnen a)




