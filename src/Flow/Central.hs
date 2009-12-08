{-# OPTIONS -fglasgow-exts #-}

module Flow.Central where

import Flow.Program
import Flow.Trace
import qualified Flow.Goto as G
import qualified Flow.Struct as S

import Challenger.Partial
import Inter.Types

import Autolib.NFA.Subseteq
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Informed

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set 

data Struct_To_Goto = Struct_To_Goto 
    deriving ( Read, Show, Typeable )

instance Partial 
        Struct_To_Goto ( Program S.Statement) 
                       ( Program G.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein goto-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = G.example

    total p i b = do
        explain_notation
        comparison 
            ( "laut Aufgabenstellung", \ l -> S.traces l i ) 
            ( "laut Ihrer Lösung", \ l -> G.traces l b )


struct_to_goto_fixed :: Make
struct_to_goto_fixed = direct Struct_To_Goto S.example

-------------------------------------------------------

comparison :: ( String, Int -> [ Trace ] )
           -> ( String, Int -> [ Trace ] )
           -> Reporter ()
comparison (s1,f1) (s2,f2) = sequence_ $ take 20 $ do
    l <- [ 0 .. ]
    let t1 = Data.Set.fromList $ f1 l
        t2 = Data.Set.fromList $ f2 l
    return $ do
        must_be_subset (s1, t1) (s2, t2)
        must_be_subset (s2, t2) (s1, t1) 

must_be_subset (s1, t1) (s2,t2) = do
    let missing = Data.Set.toList 
                $ Data.Set.difference t1 t2
    when ( not $ null missing ) $ reject $ vcat
         [ text "diese (evtl. partielle) Berechnung"
         , text "ist" <+> text s1 <+> text "möglich,"
         , text s2 <+> text "aber nicht:"
         , nest 4 $ toDoc $ head missing
         ]

-------------------------------------------------------

data Goto_To_Struct = Goto_To_Struct deriving ( Read, Show, Typeable )

instance Partial 
        Goto_To_Struct ( Program G.Statement) ( Program S.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein while-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = S.example

    total p i b = do
        explain_notation
        comparison 
            ( "laut Aufgabenstellung", \ l -> G.traces l i ) 
            ( "laut Ihrer Lösung", \ l -> S.traces l b )


goto_to_struct_fixed :: Make
goto_to_struct_fixed = direct Goto_To_Struct G.example

-------------------------------------------------------

explain_notation = inform $ vcat
   [ text "Ich vergleiche die Menge Folgen von Ereignissen,"
   , text "die bei Ausführung der Programme auftreten können."
   , text "In diesen Folgen (Wörtern) bedeutet:"
   , nest 4 $ vcat [ text "Ex = Ausführen von x,"
		   , text "Ty = Auswerten von y mit Resultat True,"
		   , text "Fy = Auswerten von y mit Resultat False."
		   ]
   ]