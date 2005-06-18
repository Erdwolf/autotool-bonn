module Code.Hamming.Partial where

--  $Id$

import Code.Hamming.Data
import Code.Hamming.Check

import Challenger.Partial
import Data.Typeable
import Autolib.ToDoc
import Autolib.Reporter

data Hamming = Hamming
    deriving (Eq, Ord, Show, Read, Typeable)

instance Partial Hamming Config Code where

    describe Hamming conf = vcat
        [ text "Gesucht ist ein Code (als Liste von W�rtern �ber L, R)"
	, text "mit diesen Eigenschaften:"
	, nest 4 $ toDoc conf
	]

    initial Hamming conf = [[L,R,L], [R,R,L]]

    partial Hamming conf code = do
        l <- equal_length code
        let (d, p) = minimum_distance code
	inform $ vcat
	       [ text "Die Hamming-Weite dieses Codes ist:" <+> toDoc d
	       , text "zwei W�rter mit diesem Abstand sind:" <+> toDoc p
	       ]

    total Hamming conf code = do
        let Just l = result $ equal_length code
            (d, _) = minimum_distance code
            helper what flag tag x v = assert (flag x v)
			    $ fsep [ text "die", toDoc what , text "soll"
				   , text tag, toDoc x, text "sein" 
				   ]
	    check what target value = case target of
                 (Fixed ,  x ) -> helper what (==) "genau"      x value
                 (Atleast, x ) -> helper what (<=) "wenigstens" x value
                 (Atmost,  x ) -> helper what (>=) "h�chstens"  x value
        check Width (Code.Hamming.Data.width conf) l
        check Size (Code.Hamming.Data.size   conf) (length code)
        check Distance (Code.Hamming.Data.distance   conf) d

instance Measure Hamming Config Code where
    measure Hamming conf code = 
        let Just l = result ( equal_length code )
            (d, _) = minimum_distance code
            s      = length code
        in  fromIntegral $ case optimize conf of
	        Width    -> l
		Size     -> s
		Distance -> d

