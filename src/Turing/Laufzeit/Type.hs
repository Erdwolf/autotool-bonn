module Turing.Laufzeit.Type where

--   $Id$

import Turing.Type (TM)

import Informed
import ToDoc
import Reader

data Laufzeit = 
     Laufzeit { fun :: Int -> Int     -- laufzeit-funktion (kein Read)
	      , fun_info :: Doc	      -- funktions-name/-erklärung
	      , args :: [ Int ]	      -- argumente zum testen
	      }

instance Informed Laufzeit where
     info = fun_info

instance ToDoc Laufzeit where
     toDoc = info
instance Show Laufzeit where
     show = render . toDoc

instance Reader Laufzeit where
     reader = error "Laufzeit.Type.reader"
instance Read Laufzeit where
     readsPrec p = error "Laufzeit.Type.readsPrec"