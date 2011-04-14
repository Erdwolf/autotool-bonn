{-# LANGUAGE DeriveDataTypeable #-}
module RM.State where

import RM.Type 
import RM.Memory

import Machine.History

import Autolib.ToDoc
import Data.Typeable

data State =
     State { schritt :: Int
	   , program :: Maybe Program
	   , memory  :: Memory
	   , past    :: [State]
	   }
     deriving ( Eq , Ord, Typeable )

instance ToDoc State where
    toDoc st = text "State" <+> dutch_record
	       [ text "schritt" <+> equals <+> toDoc ( schritt st )
	       , text "program" <+> equals <+> toDoc ( program st )
	       , text "memory" <+> equals <+> toDoc ( memory st )
	       ]

instance History State where
    history = past

