{-# LANGUAGE DeriveDataTypeable #-}
module Flow.Central where

import Flow.Program
import Flow.Trace
import Flow.Conditions
import Flow.Actions
import Flow.State
import qualified Flow.Goto as G
import qualified Flow.Struct as S

import Challenger.Partial
import Inter.Types

import Autolib.NFA.Subseteq
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Informed
import Autolib.Reporter.Set (subeq, eq)

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set 

data Struct_To_Goto = Struct_To_Goto 
    deriving ( Read, Show, Typeable )

instance OrderScore Struct_To_Goto where
    scoringOrder _ = Increasing

instance Partial 
        Struct_To_Goto ( Program S.Statement) 
                       ( Program G.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein goto-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = G.example
   
    partial p i b = 
        partializer i b

    total p i b = do
        let s = all_states
              $ Data.Set.union (conditions i) 
                               (conditions b)
        totalizer ( S.semantics s i ) 
                  ( G.semantics s b )

struct_to_goto_fixed :: Make
struct_to_goto_fixed = direct Struct_To_Goto S.example

-------------------------------------------------------

partializer i b = do
    comparing "Zustands-Prädikate" (conditions i) (conditions b)
    comparing "elementare Anweisungen" (actions i) (actions b)

comparing tag pi pb = do
    when ( pi /= pb ) $ inform $ vcat 
           [ text "Hinweis: vorkommende" <+> text tag
           , nest 4 $ text "in Aufgabenstellung" </> toDoc pi
           , nest 4 $ text "in Einsendung" </> toDoc pb
           , text "stimmen nicht überein."
           ]


totalizer orig0 this0 = do 
    orig <- orig0
    let o = informed ( text "Spursprache des Programms aus Aufgabenstellung" ) orig
    this <- this0
    let t = informed ( text "Spursprache des Programms aus Ihrer Einsendung" ) this
    ok1 <- subsetequ o t
    ok2 <- subsetequ t o
    assert ( ok1 && ok2 ) $ text "Spursprachen stimmen überein?"

-------------------------------------------------------

data Goto_To_Struct = Goto_To_Struct deriving ( Read, Show, Typeable )

instance OrderScore Goto_To_Struct where
    scoringOrder _ = Increasing

instance Partial 
        Goto_To_Struct ( Program G.Statement) ( Program S.Statement) where
    describe p i = vcat
        [ text "Gesucht ist ein while-Programm,"
	, text "das äquivalent ist zu"
	, nest 4 $ toDoc i
	]
    initial p i = S.example

    partial p i b = partializer i b

    total p i b = do
        let s = all_states
              $ Data.Set.union (conditions i) 
                               (conditions b)
        totalizer ( G.semantics s i ) 
                  ( S.semantics s b )

goto_to_struct_fixed :: Make
goto_to_struct_fixed = direct Goto_To_Struct G.example


