module Shift.Type where

-- $Id$

import Util.Size
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

--------------------------------------------------------------
 
data Meta = Meta { start :: Pins
		 , diff :: Pins
		 , qps :: [(Int,Int)]
		 }
     deriving ( Read )



instance ToDoc Meta where 
    toDoc sh = text "Meta" <+> braces ( fsep $ punctuate comma
          [ text "start" <+> equals <+> toDoc ( start sh )
	  , text "diff" <+> equals <+> toDoc ( diff sh )
	  , text "qps" <+> equals <+> toDoc ( qps sh )
          ] )

instance Show Meta where 
    show = render . toDoc

-- beispiel

me = Meta { start = [1,2,3]
	  , diff  = [0,2,2]
	  , qps = do  n <- [ 1 .. 10 ] ; return ( 0, 4*n )
	  }
