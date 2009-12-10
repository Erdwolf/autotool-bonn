{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Hanoi.Restriction where

import Condition

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Inter.Types ()

import Data.Typeable

data Restriction 
     = None 
     | Neighbours
     | Clockwise
   deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Restriction])

instance Explain Restriction where
    explain r = case r of
        None -> empty
	Neighbours -> text "Scheiben dürfen nur zwischen benachbarten Türmen bewegt werden."
	Clockwise -> text "Scheiben dürfen nur im Uhrzeigersinn bewegt werden."

    


-- local variables:
-- mode: haskell
-- end:
