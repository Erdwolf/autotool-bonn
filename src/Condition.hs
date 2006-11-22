{-# OPTIONS -fglasgow-exts #-}

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
      => Condition prop ob | ob -> prop where
    explain   :: prop -> Doc
    condition :: prop -> ob -> Reporter ()

investigate :: Condition prop ob
	    => [ prop ] -> ob -> Reporter ()
investigate props ob = sequence_ $ do 
    prop <- props
    return $ condition prop ob
