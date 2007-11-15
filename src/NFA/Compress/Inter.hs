module NFA.Compress.Inter where

import NFA.Compress.Data
import NFA.Compress.Instance
import NFA.Compress.Look

import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Autolib.Size

instance C.Partial DFA_Compress Original Compressed where
    
    describe p i = vcat
        [ text "Gesucht ist eine komprimierte Darstellung der Automatentabelle"
	, nest 4 $ toDoc $ original i
	, text "Das next/check-Array soll höchstens"
	, text "die Länge" <+> toDoc ( max_size i ) <+> text "haben".
	]

    initial p i = NFA.Compressed.Data.example

    partial p i b = do
        C.verify b
        let Original zss = original i
	sequence_ $ do
	    (x, zs) <- zip [ 0.. ] zss
	    (y, z ) <- zip [ 0 ..] zs
	    return $ silent $ do
	        t <- NFA.Compress.Look.up b x y
		inform $ text "gegeben war:" <+> toDoc z
		assert ( t == z ) $ text "stimmt überein?"
        inform $ text "alle Werte stimmen überein."
			
    total p i b = do
        assert ( size b <= max_size i ) 
	       $ text "Lösung ist klein genug?"

make :: Make
make = direct DFA_Compress NFA.Compress.Instance.example
