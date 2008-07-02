module Hanoi.Move where

--   $Id$

import Hanoi.Type
import Hanoi.Restriction
import Control.Monad

import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc

moves :: Restriction -> Hof -> [ Zug ] -> Reporter Hof
moves r = foldM (move r)

move :: Restriction -> Hof -> Zug -> Reporter Hof
move r hof zug @ (von, nach) = do
    inform $ vcat [ text "Situation:" <+> toDoc hof
		  , text "Zug:" <+> toDoc zug
		  ]
    check r hof zug

    silent $ assert ( von `elem` keysFM hof )
	   $ fsep [ text "Start-Turm", toDoc von, text "erlaubt?" ]
    let Just from = lookupFM hof von

    silent $ assert ( not $ null from ) 
	   $ text "Start-Turm nicht leer?"
    let top : rest = from
    silent $ inform $ fsep 
	            [ text "oberste Scheibe auf Start-Turm ist", toDoc top ]

    silent $ assert ( nach `elem` keysFM hof )
	   $ fsep [ text "Ziel-Turm", toDoc nach , text "erlaubt?" ]
    let Just to = lookupFM hof nach

    silent $ case to of
         [] -> inform $ text "Ziel-Turm ist leer."
	 ( op : _ ) -> do
	     inform $ fsep 
			     [ text "oberste Scheibe auf Ziel-Turm ist"
			     , toDoc op ]
             assert ( top < op ) $ text "groß genug?"

    return $ addListToFM hof
	   [ ( von, rest ) , ( nach, top : to ) ]


check :: Restriction 
      -> Hof 
      -> Zug 
      -> Reporter ()
check r hof ( von, nach ) = 
    let v = position hof von
        n = position hof nach
    in  case r of
        None -> return ()
        Neighbours -> silent $ assert ( 1 == abs ( v - n ) )
			 $ text "Diese Türme sind nicht benachbart."
        Clockwise  -> silent $ assert ( succ v `mod` sizeFM hof == n ) 
			 $ text "Zug geht nicht zum Nachbarn im Uhrzeigersinn."

position :: Hof -> Turm -> Int
position hof t = length $ takeWhile ( /= t ) $ keysFM hof
