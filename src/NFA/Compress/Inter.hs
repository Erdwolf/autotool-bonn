module NFA.Compress.Inter where

import NFA.Compress.Compressed
import NFA.Compress.Instance
import NFA.Compress.Data
import NFA.Compress.Look
import NFA.Compress.Roll
import NFA.Compress.Config

import qualified Fun.Table as F


import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Inter.Quiz
import Autolib.Size

import Data.Typeable
import Data.Array (array)

instance C.Partial DFA_Compress Instance Compressed where
    
    describe p i = vcat
        [ text "Gesucht ist eine komprimierte Darstellung der Automatentabelle"
	, nest 4 $ toDoc $ tafel $ original i
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

tafel zss =
        let r = fromIntegral $ length zss
            c = fromIntegral $ length $ head zss
        in  F.Tafel2 $ array ((0,0), (r-1,c-1)) $ do
                    ( x, zs ) <- zip [0..] zss
                    ( y, z ) <- zip [ 0..] zs
                    return ((x,y), fromIntegral z)
        


make_fixed :: Make
make_fixed = direct DFA_Compress NFA.Compress.Instance.example

instance Generator DFA_Compress Config Instance where
    generator p conf key = roll conf

instance Project DFA_Compress Instance Instance where
    project p = id    


make_quiz :: Make
make_quiz = quiz DFA_Compress NFA.Compress.Config.example
