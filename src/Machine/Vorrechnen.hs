module Machine.Vorrechnen

where

-- $Id$

import Machine.Class

import Set
import Schichten
import Reporter
import ToDoc

vorrechnen :: Machine m dat conf
	   => m -> dat
	   -> Reporter ()
vorrechnen a xs = vorrechnen_cut 10 a xs

vorrechnen_cut :: Machine m dat conf
	   => Int 
	   -> m -> dat
	   -> Reporter ()
vorrechnen_cut cut a xs = do
    inform $ fsep [ text "mit Eingabe", toDoc xs
		  , text "erreicht die Maschine folgende Konfigurationen:"
		  , nest 8 $ parens (text "ich zeige maximal die ersten" 
			    <+> toDoc cut <+> text "Schritte")
		  ]
    inform $ nest 4 $ vcat $ do
        (i, ks) <- zip [0 :: Int .. ] 
		$ take cut
		$ map setToList 
		$ schichten ( next a )
		$ input a xs
	return  $ text "nach" <+> toDoc i <+> text "Schritten:"
	        <+> ( vcat $ do k <- ks; return $ toDoc k )

    
vorrechnens :: Machine m dat conf
	   => m -> [ dat ]
	   -> Reporter ()
vorrechnens a = mapM_ (vorrechnen a)




