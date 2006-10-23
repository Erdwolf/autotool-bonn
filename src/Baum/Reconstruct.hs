{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances #-}

module Baum.Reconstruct where

import Baum.Type
import Baum.Config
import Baum.Traverse
import Baum.Label
import Baum.Bin
import Baum.Roll

import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Xml
import Autolib.Util.Seed

import Inter.Types
import Inter.Quiz
import Data.Typeable

data Reconstruct = Reconstruct
     deriving ( Eq, Ord, Show, Read, Typeable )

type Traversals c = [ ( Order, [ c ]  ) ]



instance ( Symbol c, ToDoc [c], Read c ) 
       => Partial Reconstruct ( Traversals c ) ( Term () c ) 
    where
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
	     in	 Baum.Label.label Pre t $ reverse cs

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

-- | if inorder is involved, then tree must be binary
onlybin :: Traversals c -> Bool
onlybin ts = or $ do ( o, cs ) <- ts ; return ( o == In )

make_fixed :: Make
make_fixed = 
    let ts :: Traversals Identifier
	ts = [ ( Pre , read "[ e, j, b, i, f, m, l, k, d, g, c, a, h ]" )
	     , ( In  , read "[ b, j, i, e, k, l, d, m, g, f, a, c, h ]" )
	     ]
    in  direct Reconstruct ts

instance Container Identifier String where
    label _ = "Identifier"
    pack = show
    unpack = read
           
make_quiz :: Make
make_quiz = quiz Reconstruct Baum.Config.example

instance Generator Reconstruct Config ( Traversals Identifier ) where
    generator Reconstruct conf key = do
          seed $ read key + hash conf
	  t <- roll $ nodes conf
          return $ do 
		 o <- orders conf
		 return ( o , traverse o t )

instance Project Reconstruct  
                 ( Traversals Identifier )  ( Traversals Identifier ) where
    project Reconstruct t = t
