module Turing.Vorrechnen

where

-- -- $Id$

import Turing
import Turing.Konfiguration
import Turing.Nachfolger
import Schichten

import Reporter
import ToDoc

vorrechnen :: TUM y z
	   => Turing y z -> [y]
	   -> Reporter ()
vorrechnen a xs = do
    vorrechnen_cut 10 a xs

vorrechnen_cut :: TUM y z
	   => Int 
	   -> Turing y z -> [y]
	   -> Reporter ()
vorrechnen_cut cut a xs = do
    inform $ fsep [ text "mit Eingabe", toDoc xs
		  , text "erreicht der Automat folgende Konfigurationen:"
		  , nest 8 $ parens (text "ich zeige maximal die ersten" 
			    <+> toDoc cut <+> text "Schritte")
		  ]
    inform $ nest 4 $ vcat $ do
        (i, ks) <- zip [0 :: Int .. ] 
		$ take cut
		$ map setToList 
		$ schichten ( folgekonfigurationen a )
		$ start_konfiguration a xs
	return  $ text "nach" <+> toDoc i <+> text "Schritten:"
	        <+> ( vcat $ do k <- ks; return $ toDoc k )

    
vorrechnens :: TUM y z
	   => Turing y z -> [[y]]
	   -> Reporter ()
vorrechnens a xss = do
    inform $ text "rechnen für eingaben" <+> toDoc xss
    mapM_ (vorrechnen a) xss




