module Hanoi.Move where

--   $Id$

import Hanoi.Type
import Control.Monad

import Data.FiniteMap
import Reporter
import ToDoc

moves :: Hof -> [ Zug ] -> Reporter Hof
moves = foldM move

move :: Hof -> Zug -> Reporter Hof
move hof zug @ (von, nach) = do
    inform $ vcat [ text "Situation:" <+> toDoc hof
		  , text "Zug:" <+> toDoc zug
		  ]
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
