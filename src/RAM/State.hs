module RAM.State where

-- -- $Id$

import RAM.Type
import RAM.Memory
import Machine.History

import ToDoc

data State =
     State { schritt :: Int
	   , memory :: Memory
	   , todo   :: Program -- noch auszuführen
	   , past :: [State] -- vorige zustände
	   }
     deriving ( Eq, Ord )

instance ToDoc State where
    toDoc s = text "State" <+> dutch_record 
	    [ text "schritt" <+> equals <+> toDoc ( schritt s )
	    , text "memory" <+> equals <+> toDoc ( memory s )
	    , text "todo" <+> equals 
	      <+> clipped_dutch_list 1 ( map toDoc $ todo s )
	    ]

instance History State where
    history = past

