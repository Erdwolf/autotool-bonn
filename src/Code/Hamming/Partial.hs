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
	    check tag target value = case target of
                 Fixed x -> assert ( x == value )
			    $ fsep [ text "die", text tag
				   , text "soll", toDoc x, text "sein" 
				   ]
		 _ -> return ()
        check "Länge" (Code.Hamming.Data.length conf) l
        check "Größe" (size   conf) (Prelude.length code)
        check "Weite" (dist   conf) d


instance Measure Hamming Config Code where
    measure Hamming conf code = 
        let Just l = result ( equal_length code )
            (d, _) = minimum_distance code
            s      = Prelude.length code
            meter (f, v) conf = do
                case f conf of
		    Fixed _ -> mzero
		    _       -> return v
        in  head $ do 
	        (f, v) <- [ (Code.Hamming.Data.length, l)
			  , (dist, d), (size, s) 
			  ]
                meter (f, v) conf  

