module Code.Class where

--  $Id$

import Code.Type

import Challenger.Partial
import ToDoc
import Reporter


data Encode a b = Encode ( Coder a b )

instance Show ( Encode a b ) where
    show ( Encode c ) = "Encode"

instance ( ToDoc [a], Eq b ) 
	 => Partial ( Encode a b ) [a] b where

    describe ( Encode p ) i = vcat    
        [ text "Gesucht ist das Ergebnis der Kodierung von"
	, nest 4 $ toDoc i
	, text "nach dem Verfahren"
	, nest 4 $ nametag p
	]

    initial ( Encode p ) i = encode p $ take 2 i

    total ( Encode p ) i b = do
        let out = encode p i
	if ( b == out ) 
	   then inform $ text "Das Ergebnis ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."


data Decode a b = Decode ( Coder a b ) 

instance Show ( Decode a b ) where
    show (Decode c) = "Decode"

instance ( ToDoc b, Eq b  )
	 => Partial ( Decode a b ) b [ a ] where

    describe ( Decode p ) i = vcat
        [ text "Gesucht ist eine Eingabe,"
	, text "aus der das Verfahren" <+> nametag p 
	, text "diese Ausgabe erzeugt:"
	, nest 4 $ toDoc i
	]

    initial ( Decode p ) i = decode_hint p i

    total ( Decode p ) i b = do
        let out = encode p b
	if ( i == out ) 
	   then inform $ text "Die Eingabe ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."
	 
