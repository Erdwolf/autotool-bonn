obsolete module NPDA.Vorrechnen
-- use Machine.Vorrechnen instead

-- $Id$

where

import NPDA.Type
import NPDA.Konfiguration
import NPDA.Nachfolger

import Schichten
import ToDoc
import Reporter

vorrechnen :: (Ord x, Ord y, Ord z, 
	   ToDoc x, ToDoc y, ToDoc z
	   ,	 ToDoc [x], ToDoc [y], ToDoc [z]) 
	   => NPDA x y z -> [x] 
	   -> Doc
vorrechnen a xs = 
 let cut = 5
 in  fsep $
     [ ( text $ "mit Eingabe " ) <+> toDoc xs 
     , text $ "erreicht der Automat folgende Konfigurationen"
     , text $ "(ich zeige nur die ersten " ++ show cut ++ " schritte)"
     , nest 4 $ fsep $ do
        (i, ks) <- zip [0..] 
		$ map setToList 
		$ take cut
		$ schichten (folgekonfigurationen a   )
		$ start_konfiguration a xs
	return $ text ( "nach " ++ show i ++ " Schritten:" )
	       <+> nest 4 ( fsep $ do k <- ks; return $ toDoc k )
    ]
    
vorrechnens :: (Ord x, Ord y, Ord z, 
	   ToDoc x, ToDoc y, ToDoc z
	   ,	 ToDoc [x], ToDoc [y], ToDoc [z]) 
	   => NPDA x y z -> [[x]] 
           -> Reporter ()
vorrechnens a xss = 
    inform $ vcat $ map ( vorrechnen a ) xss


