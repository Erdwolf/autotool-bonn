-- $Id$

module Grammatik.Type 

( module Grammatik.Type
, module Set
)

where


import Set
import ReadSet

import Size
import ToDoc

data Grammatik = Grammatik
	       { terminale      :: Set Char
	       , nichtterminale :: Set Char
	       , startsymbol	:: Char
	       , regeln		:: Set (String, String) 
	       }
     deriving (Read,Show)

terms = setToList . terminale
vars  = setToList . nichtterminale
rules = setToList . regeln

-- instance Show Grammatik where show = render . toDoc

instance Size Grammatik where size = cardinality . regeln

instance ToDoc Grammatik where
    toDoc a = 
	 let pterm = text "terminale" <+> equals <+> toDoc (terminale a)
	     pnicht = text "nichtterminale" <+> equals <+> toDoc (nichtterminale a)
	     pstart = text "startsymbol" <+> equals <+> toDoc (startsymbol a)
	     pregeln  = text "regeln" <+> equals <+> toDoc (regeln a)

	 in      text "Grammatik" 
	     <+> braces ( fsep $ punctuate comma 
				    [ pterm, pnicht, pstart, pregeln] )




