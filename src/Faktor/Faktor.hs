-- | Korrekturfunktion für Faktorisierung

-- joe@informatik.uni-leipzig.de
-- benutzt code für challenger/PCProblem
-- von Markus Kreuz  mai99byv@studserv.uni-leipzig.de

module Faktor.Faktor (
     Faktor (..)
    ) where

--  $Id$

import Challenger.Partial
import ToDoc
import Reporter
import Ana
import Data.Typeable

-- import Number
-- import Iso
-- import System


data Faktor = Faktor deriving ( Show, Typeable )

-- instance Number Integer Integer where number = id

instance Partial Faktor Integer ( Integer, Integer ) where

    describe Faktor x = vcat
	   [ text "Gesucht sind Zahlen (y, z) mit y > 1, z > 1"
	   , text "und y * z =" <+> toDoc x
	   ]

    initial Faktor x = 
        let -- do something silly to get candidates of useful size
            b = 3
	    xs = based b x
	    (ys, zs) = splitAt (length xs `div` 2) xs
	in  (unbased b ys, unbased b zs)

    partial Faktor x (y, z) = do
           -- assert (x > 1) $ text "Die Zahl soll > 1 sein."
	   assert (y > 1 && z > 1) $ text "Beide Zahlen sollen > 1 sein."

    total Faktor x (y, z) = do
        let yz = y * z  
        assert (x == yz) $ fsep
     	                      [ text "Das Produkt der beiden Zahlen"
			      , toDoc y, text "und", toDoc z
			      , text "ist" , toDoc yz
			      , text "und nicht", toDoc x
			      ]



