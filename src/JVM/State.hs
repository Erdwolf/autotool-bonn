module JVM.State where

--   $Id$

import JVM.Type
import JVM.Memory

import Machine.History
import Data.Array

import ToDoc

data State =
     State { schritt :: Int
	   , code :: Array Int Statement
	   , pc   :: Int
	   , stack :: [ Integer ]
	   , memory  :: Memory
	   , past :: [State] -- vorige zustände
	   }
     deriving ( Eq, Ord )

instance ToDoc State where
    toDoc st = text "State" <+> dutch_record 
	    [ text "schritt" <+> equals <+> toDoc ( schritt st )
	    , text "memory" <+> equals <+> toDoc ( memory st )
	    , text "stack" <+> equals <+> toDoc ( stack st )
	    , text "pc" <+> equals <+> toDoc ( pc st )
	    , text "code [PC]" <+> equals <+> 
	         if   inRange (bounds $ code st) ( pc st )
	         then toDoc ( code st ! pc st )
	         else text "<<outside>>"
	    ]

instance History State where
    history = past

