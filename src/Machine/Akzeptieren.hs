module Machine.Akzeptieren 

where

-- -- $Id$

import Machine.Class
-- import Machine.Vorrechnen
import Machine.History

import Monad (guard)
import Reporter hiding ( output )
import ToDoc
import Set

akzeptierend
    :: Machine m dat conf
    => Int -> m -> dat -> [ conf ]
akzeptierend cut m xs = do
    k <- akzeptierend_oder_ohne_nachfolger cut m xs
    guard $ accepting m k
    return k

akzeptierend_oder_ohne_nachfolger
    :: Machine m dat conf
    => Int -> m -> dat -> [ conf ]
akzeptierend_oder_ohne_nachfolger cut m xs = do
    k <- take cut $ nachfolger m $ input m xs
    guard  $ isEmptySet $ next m $ k
    return k

check_liste ::  ( Machine m dat conf, ToDoc [dat] )
	      => Bool -- soll akzeptieren?
	      -> Int -> m -> [dat]
	      -> Reporter ()
check_liste acc cut m xs = mapM_ ( check_item acc cut m ) xs

check_item ::   ( Machine m dat conf, ToDoc dat )
	      => Bool -- soll akzeptieren?
	      -> Int -> m -> dat
	      -> Reporter ()
check_item acc cut m x = nested 4 $ do
     let ks = akzeptierend_oder_ohne_nachfolger cut m x
	 as = filter ( accepting m ) ks
	 logs = if null as then ks else as
	 ok = acc == not (null as)
     let msg = vcat
	     [ fsep [ text ( if ok then "Richtig:" else "Falsch:" )
		    , text "die Eingabe" <+> toDoc x
		    , text "wurde"
		    , text ( if null as then "nicht" else "" ) 
		    , text "akzeptiert."
		    ]
	     , text "einige Rechnungen der Maschine sind:"
	     , nest 4 $ vcat $ take 3 $ map present $ logs
	     ]
     if ok then inform msg
	   else reject msg

positiv_liste :: ( Machine m dat conf, ToDoc [dat] )
	      => Int -> m -> [dat]
	      -> Reporter ()
positiv_liste cut m  xss = do
    inform $ vcat
	   [ text "Ich prüfe, daß jede dieser Eingaben akzeptiert wird:"
	   , nest 4 $ toDoc xss
	   ]
    check_liste True cut m xss

negativ_liste :: ( Machine m dat conf, ToDoc [dat] )
	      => Int -> m -> [dat] 
              -> Reporter ()
negativ_liste cut m xss = do
    inform $ vcat
	   [ text "Ich prüfe, daß keine dieser Eingaben akzeptiert wird:"
	   , nest 4 $ toDoc xss
	   ]
    check_liste False cut m xss




