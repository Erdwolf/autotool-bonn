module NPDA.Akzeptieren 

-- $Id$

where

import NPDA.Type
import NPDA.Konfiguration
import NPDA.Nachfolger
import NPDA.Vorganger
import NPDA.Vorrechnen

import Reporter
import ToDoc
import Monad (guard)


akzeptierend :: (Ord x, Ord y, Ord z)
    => Int -> NPDA x y z -> [x] -> [ Konfiguration x y z ]
akzeptierend cut a xs = do
    k <- take cut $ nachfolger a (start_konfiguration a xs)
    guard $ eingabe k == []
    guard $ case akzeptiert a of
	  Leerer_Keller -> keller k == []
	  Zustand zs -> zustand k `elementOf` zs
    return k

----------------------------------------------------------------------


mit_leerem_keller_akzeptierte_eingaben
    :: (Ord x, Ord y, Ord z)
    => NPDA x y z -> [ Konfiguration x y z ]
mit_leerem_keller_akzeptierte_eingaben a = take 10 $ do
    let ks = leere_keller_konfigurationen a
    k <- take 1000 $ vorganger a ks
    guard $ keller k == [ startsymbol a ]
    guard $ zustand k == startzustand a
    return k


----------------------------------------------------------------------

ja :: Bool -> String
ja True = "" ; ja False = "nicht"

nein :: Bool -> String
nein f = ja ( not f )

liste :: (Ord x, Ord y, Ord z,
	 	  ToDoc x, ToDoc y, ToDoc z
	   ,	 ToDoc [x], ToDoc [y], ToDoc [z]) 
	      => Bool
	      -> Int 
	      -> NPDA x y z -> [[x]] 
	      -> Reporter ()
liste acc cut a xss = case take 3 $
    do xs <- xss
       let ks = akzeptierend cut a xs
       guard $ acc == null ks -- d. h. fälschlicherweise nicht akzeptiert
       return xs
  of []  -> inform $ text 
	    $ unwords [ "alle Wörter wurden", ja acc, "akzeptiert" ]
     xss -> reject $ vcat 
		  $ [ text $ unwords [ "diese Wörter wurden", nein acc, "akzeptiert:" ]
		    , nest 4 $ toDoc xss 
		    , nest 4 $ vcat $ map (vorrechnen a) xss 
		    ] 
		    

----------------------------------------------------------------------

positiv_liste, negativ_liste :: (Ord x, Ord y, Ord z,
	 	  ToDoc x, ToDoc y, ToDoc z
	   ,	 ToDoc [x], ToDoc [y], ToDoc [z]) 
	      => Int 
	      -> NPDA x y z -> [[x]] 
	      -> Reporter ()
positiv_liste cut a xss = liste True  cut a xss
negativ_liste cut a xss = liste False cut a xss


