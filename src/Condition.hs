{-# OPTIONS -fglasgow-exts #-}
{-# language UndecidableInstances, OverlappingInstances #-}

module Condition 

( module Condition
, module Suggest
)

where

import Suggest

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

class ( ToDoc prop, Reader prop, Typeable prop, Suggest prop ) 
    => Explain prop where
    explain   :: prop -> Doc
    explain = toDoc
   
instance ( ToDoc prop, Reader prop, Typeable prop, Suggest prop ) 
    => Explain prop 

class Explain prop
      => Condition prop ob | ob -> prop where
    condition :: prop -> ob -> Reporter ()

investigate :: Condition prop ob
	    => [ prop ] -> ob -> Reporter ()
investigate props ob = sequence_ $ do 
    prop <- props
    return $ condition prop ob
