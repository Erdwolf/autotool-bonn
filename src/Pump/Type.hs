module Pump.Type where

-- $Id$

import Reporter
import ToDoc
import Size
import FiniteMap
import ReadFM

--------------------------------------------------------------------------

class ( Ord z,  Show z, ToDoc z, ToDoc [z] ) => Pumping z where
    tag :: z -> String
    tag_show :: z -> String
    admissable :: Int -> z -> Reporter ()
    inflate_show :: Int -> z -> String
    inflate_show_i :: z -> String

    inflate :: Int -> z -> String
    zerlegungen :: String -> Int -> [ z ]

    exempel :: z

--------------------------------------------------------------------------

data Pumping z => 
     Pump z = Nein
	     { wort :: FiniteMap Int String }
	    | Ja
	     { n :: Int
	     , zerlege :: FiniteMap String z
	     }
     deriving ( Read )

instance Pumping z => Show ( Pump z ) where show = render . toDoc

instance Pumping z => Size ( Pump z ) where
    size p @ Nein {} = sum [ length $ w | (n,w) <- fmToList $ wort p ]
    size p @ Ja   {} = n p

instance Pumping z => ToDoc ( Pump z ) where 
    toDoc ( p @ Nein {} ) = 
	  text "Nein" <+> braces ( fsep $ punctuate comma
	       [ text "wort" <+> equals <+> toDoc ( wort p )
	       ] )
    toDoc ( p @ Ja   {} ) = 
	  text "Ja" <+> braces ( fsep $ punctuate comma
	       [ text "n" <+> equals <+> toDoc (n p)
	       , text "zerlege" <+> equals <+> toDoc (zerlege p)
	       ] )
