module RAM.State where

-- $Id$

import RAM.Type
import RAM.Memory

import ToDoc

data State =
     State { memory :: Memory
	   , todo   :: Program -- noch auszuführen
	   }
     deriving ( Eq, Ord )

instance ToDoc State where
    toDoc s = text "State" <+> dutch_record 
	    [ text "memory" <+> equals <+> toDoc ( memory s )
	    , text "todo" <+> equals 
	      <+> clipped_dutch_list 1 ( map toDoc $ todo s )
	    ]
