module Code.Hamming.Partial where

--  $Id$

import Code.Hamming.Data
import Code.Hamming.Check

import Challenger.Partial

import ToDoc
import Reporter

data Hamming = Hamming
    deriving (Eq, Ord, Show, Read)

instance Partial Hamming Config Code where

    describe Hamming conf = vcat
        [ text "Gesucht ist ein Code (als Liste von Wörtern über L, R)"
	, text "mit diesen Eigenschaften:"
	, nest 4 $ toDoc conf
	]

    initial Hamming conf = [[L,R,L], [R,R,L]]

    partial Hamming conf code = do
        l <- equal_length code
        let (d, p) = minimum_distance code
	inform $ vcat
	       [ text "Die Hamming-Weite dieses Codes ist:" <+> toDoc d
	       , text "zwei Wörter mit diesem Abstand sind:" <+> toDoc p
	       ]

    total Hamming conf code = do
        let Just l = result $ equal_length code
            (d, _) = minimum_distance code
            helper what flag tag x v = assert (flag x v)
			    $ fsep [ text "die", text what , text "soll"
				   , text tag, toDoc x, text "sein" 
				   ]
	    check what target value = case target of
                 (Fixed ,  x ) -> helper what (==) "genau"      x value
                 (Atleast, x ) -> helper what (<=) "wenigstens" x value
                 (Atmost,  x ) -> helper what (>=) "höchstens"  x value
        check "Länge" (Code.Hamming.Data.length conf) l
        check "Größe" (size   conf) (Prelude.length code)
        check "Weite" (distance   conf) d

    measure Hamming conf code = 
        let Just l = result ( equal_length code )
            (d, _) = minimum_distance code
            s      = Prelude.length code
        in  case optimize conf of
	        "Länge" -> l
		"Größe" -> s
		"Weite" -> d

