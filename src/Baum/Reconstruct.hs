module Baum.Reconstruct where

--  $Id$

import Baum.Type
import Baum.Traverse
import Baum.Label
import Baum.Bin

import Challenger.Partial
import Reporter
import ToDoc
import Reader

data Reconstruct = Reconstruct
     deriving (Eq, Ord, Show, Read)

instance ToDoc Reconstruct where toDoc = text . show

type Traversals c = [ ( Order, [ c ]  ) ]

instance ( Eq c, Show c, ToDoc c, ToDoc [c], Reader c ) =>
    Partial Reconstruct ( Traversals c ) ( Term () c ) where
        describe Reconstruct ocs = vcat
	  [ hsep [ text "Gesucht ist ein"
		 , if onlybin ocs then text "binärer" else empty
		 , text "Baum t mit den Knoten-Reihenfolgen:"
		 ]
	  , nest 4 $ vcat $ do
	         ( o, cs ) <- ocs
		 return $ announce o ( text "t" ) <+> toDoc cs
	  ]
	initial Reconstruct traversals = 
	     let ( o, cs ) = head traversals
		 t = balanced $ length cs
	     in	 label Pre t $ reverse cs

	partial Reconstruct traversals t = do
	     inform $ text "Ihr Baum t ist" <+> present t
	     checkbin t
	     
	total Reconstruct soll t = do
	     let ocds = do 
		     ( o, cs ) <- soll
		     let ds = traverse o t 
		     return ( o, cs, ds )
	     inform $ vcat $ do
		 ( o, cs, ds ) <- ocds
		 return $ vcat 
			[ announce o empty
			, nest 4 $ vcat 
				 [ announce o ( text "Eingabe" ) <+> toDoc ds
				 , announce o ( text "Gesucht" ) <+> toDoc cs 
				 , text "stimmen überein?" 
				     <+> toDoc ( cs == ds )
				 ]
			]
	     let ok = and $ do ( o, cs, ds ) <- ocds ; return ( cs == ds )
	     assert ok $ text "stimmt alles überein?"

onlybin :: Traversals c -> Bool
-- if inorder is involved, then tree must be binary
onlybin ts = or $ do ( o, cs ) <- ts ; return ( o == In )

