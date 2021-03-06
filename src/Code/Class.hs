{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Code.Class where


import Code.Type
import Code.Param
import Inter.Types

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Reporter

import Data.Typeable

instance ( ToDoc c, Reader c, ToDoc a, Reader a, Coder c a b, Read b 
         )
	 => Partial ( Encode c ) [ a ] b where

    describe ( Encode c ) i = vcat    
        [ text "Gesucht ist das Ergebnis der Kodierung von"
	, nest 4 $ toDoc i
	, text "nach dem Verfahren"
	, nest 4 $ toDoc c
	]

    initial ( Encode c ) i = encode c $ take 2 i

    total ( Encode c ) i b = do
        out <- silent $ encodeR c i
	if ( b == out ) 
	   then inform $ text "Das Ergebnis ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."

instance BitSize b => Measure ( Encode c ) [ a ] b  where
    measure ( Encode c ) xs b = bitSize b

instance OrderScore ( Encode c ) where
    scoringOrder _ = None

enc :: ( Reader c, Reader b, ToDoc c, Coder c Char b, Read b ) => c -> Make
enc c = direct (Encode c) "abracadabra"

instance ( ToDoc c, Reader c, ToDoc a, Reader a, Coder c a b, Read b 
         )
	 => Partial ( Decode c ) b [ a ] where

    describe ( Decode c ) i = vcat
        [ text "Gesucht ist eine Eingabe,"
	, text "aus der das Verfahren" <+> toDoc c 
	, text "diese Ausgabe erzeugt:"
	, nest 4 $ toDoc i
	]

    initial ( Decode c ) i = decode_hint c i

    total ( Decode c ) i b = do
        out <- silent $ encodeR c b
	if ( i == out ) 
	   then inform $ text "Die Eingabe ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."
	 

dec :: (Reader b, Reader c, ToDoc c, ToDoc a, Reader a, Coder c a b , Show b, Read b 
       ) 
       => c -> b -> Make
dec c b = direct (Decode c) b

instance Measure ( Decode c ) b [ a ]  where
    measure ( Decode c ) b xs = fromIntegral $ length xs

instance OrderScore ( Decode c ) where
    scoringOrder _ = None
