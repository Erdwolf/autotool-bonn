module Condition where

import Autolib.Reporter
import Autolib.ToDoc

class Condition prop ob | prop -> ob, ob -> prop where
    explain   :: prop -> Doc
    condition :: prop -> ob -> Reporter ()

investigate :: ( ToDoc prop, Condition prop ob )
	    => [ prop ] -> ob -> Reporter ()
investigate props ob = sequence_ $ do 
    prop <- props
    return $ condition prop ob
