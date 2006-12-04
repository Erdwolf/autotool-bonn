{-# OPTIONS -fglasgow-exts #-}

module Flow.Central where

import Flow.Program
import qualified Flow.Goto as G
import qualified Flow.Struct as S

import Challenger.Partial
import Inter.Types

import Autolib.NFA.Subseteq
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Informed

import Data.Typeable

data Struct_To_Goto = Struct_To_Goto deriving ( Read, Show, Typeable )

instance Partial 
        Struct_To_Goto ( Program S.Statement) ( Program G.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein goto-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = G.example
    total   p i b = do
	explain_notation
        orig <- S.semantics i
	let o = informed ( text "Programm aus Aufgabenstellung" ) orig
	this <- G.semantics b
	let t = informed ( text "Programm aus Einsendung" ) this
	ok1 <- subsetequ o t
	ok2 <- subsetequ t o
	assert ( ok1 && ok2 ) $ text "OK"

struct_to_goto_fixed :: Make
struct_to_goto_fixed = direct Struct_To_Goto S.example

------------------------------------------------------------------------

data Goto_To_Struct = Goto_To_Struct deriving ( Read, Show, Typeable )

instance Partial 
        Goto_To_Struct ( Program G.Statement) ( Program S.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein while-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = S.example
    total   p i b = do
	explain_notation
        orig <- G.semantics i
	let o = informed ( text "Programm aus Aufgabenstellung" ) orig
	this <- S.semantics b
	let t = informed ( text "Programm aus Einsendung" ) this
	ok1 <- subsetequ o t
	ok2 <- subsetequ t o
	assert ( ok1 && ok2 ) $ text "OK"

goto_to_struct_fixed :: Make
goto_to_struct_fixed = direct Goto_To_Struct G.example

explain_notation = inform $ vcat
   [ text "Ich vergleiche die Menge Folgen von Ereignissen,"
   , text "die bei Ausführung der Programme auftreten können."
   , text "In diesen Folgen (Wörtern) bedeutet:"
   , nest 4 $ vcat [ text "Ex = Ausführen von x,"
		   , text "Ty = Auswerten von y mit Resultat True,"
		   , text "Fy = Auswerten von y mit Resultat False."
		   ]
   ]