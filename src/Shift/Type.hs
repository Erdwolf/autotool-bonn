module Shift.Type where

-- $Id$

import Size
import ToDoc

type Pin = Int
type Pins = [ Pin ]

data Shift = Shift { pins :: Pins
		   , vorperiode :: Int
		   , periode :: Int
		   }
    deriving ( Eq, Ord, Read )

instance Size Shift where size = periode

instance ToDoc Shift where 
    toDoc sh = text "Shift" <+> braces ( fsep $ punctuate comma
          [ text "pins" <+> equals <+> toDoc ( pins sh )
	  , text "vorperiode" <+> equals <+> toDoc ( vorperiode sh )
	  , text "periode" <+> equals <+> toDoc ( periode sh )
          ] )

instance Show Shift where 
    show = render . toDoc

-- beispiel

sh = Shift { pins = [1,10,19,20]
	   , vorperiode = 0
	   , periode = 181
	   }
 
