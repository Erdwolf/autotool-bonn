--  $Id$


module Partition.Central 

( Partition
, make_fixed
, make_quiz
)

where

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Partition.Beispiel
import Partition.Roll

import Autolib.ToDoc
import Autolib.Set

import Autolib.Reporter

instance Partial Partition 
	         ( Set Integer ) 
	         ( Set Integer, Set Integer ) where

    describe Partition s = vcat
        [ text "Zerlegen Sie die Menge"
	, nest 4 $ toDoc s
        , text "in zwei Teilmengen mit übereinstimmender Element-Summe."
	]

    initial Partition s = 
        let dist :: [a] -> ( [a], [a] )
            dist [] = ( [], [] )
	    dist (x : xs) = let (here, there) = dist xs 
                            in  (x : there, here)
            ( this, that ) = dist $ toList s
	in  ( mkSet this
	    , mkSet that 
	    )

    partial Partition s0 (l0, r0) = do
        let s = informed ( text "S" ) s0
	    l = informed ( text "L" ) l0
	    r = informed ( text "R" ) r0
        equals s ( plus l r )

    total Partition s (l, r) = do
        sl = sum $ toList l
	sr = sum $ toList r
        inform $ vcat
	       [ text "sum" <+> parens ( toDoc l ) 
	       , nest 4 $ text "=" <+> toDoc sl
	       , text "sum" <+> parens ( toDoc r ) 
	       , nest 4 $ text "=" <+> toDoc sr
	       ]
	assert ( sl == sr )
	       $ text "Stimmen die Summen überein?"

make_fixed :: Make
make_fixed = direct Partition Partition.Beispiel.mm

instance Generator  Partition 
	         ( Set Integer ) 
	         ( Set Integer, Set Integer ) where

    generator Partition conf key = hgen2 conf

instance Project  Partition 
	         ( Set Integer ) 
	         ( Set Integer,  Set Integer ) where

    project Partition ( f, b ) = f

make_quiz :: Make
make_quiz = quiz Partition $ Partition.Param.p 5





