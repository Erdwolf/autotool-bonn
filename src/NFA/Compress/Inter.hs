module NFA.Compress.Inter where

import NFA.Compress.Compressed
import NFA.Compress.Instance
import NFA.Compress.Data
import NFA.Compress.Look
import NFA.Compress.Quiz

import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Autolib.Size

instance C.Partial DFA_Compress Instance Compressed where
    
    describe p i = vcat
        [ text "Gesucht ist eine komprimierte Darstellung der Automatentabelle"
	, nest 4 $ toDoc $ original i
	, text "Das next/check-Array soll höchstens"
	, text "die Länge" <+> toDoc ( max_size i ) <+> text "haben."
	]

    initial p i = NFA.Compress.Compressed.example

    partial p i b = do
        C.verify p b
	sequence_ $ do
	    (x, zs) <- zip [ 0 .. ] $ original i
	    (y, z ) <- zip [ 0 .. ] zs
	    return $ silent $ do
		inform $ hsep [ text "lookup" , toDoc (x,y), text "=?=", toDoc z ]
	        t <- NFA.Compress.Look.up b x y
		assert ( t == z ) $ text "Wert ist korrekt?"
        inform $ text "alle Werte sind korrekt."
			
    total p i b = do
        assert ( size b <= max_size i ) 
	       $ text "Lösung ist klein genug?"

make :: Make
make = direct DFA_Compress NFA.Compress.Instance.example

instance Generator DFA_Compress Config Instance where
    generator p conf key = roller conf

instance Project DFA_Compress Instance Instance where
    project = id    
