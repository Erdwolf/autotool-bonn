module NFA.Nerode.Congruent.Quiz where

import qualified NFA.Nerode.Congruent.Config as C
import qualified NFA.Nerode.Congruent.Instance as I
import qualified NFA.Roll 

import NFA.Property
import NFA.Infinite

import qualified Convert.Language 
import qualified Convert.Input

import NFA.Nerode.Congruent.Check

import Inter.Quiz
import Inter.Types

import Autolib.Set
import Autolib.NFA
import Autolib.Util.Zufall

instance NFAC c Int => Generator 
        Nerode_Congruent ( C.Config c ) ( I.Instance c ) where
    generator p conf key = do
        a <- NFA.Roll.roll 
               [ Alphabet $ C.alphabet conf
               , Max_Size $ C.nondet_automaton_size conf
               ]
        let d = Autolib.NFA.minimize0 a
            -- gibt wenigstens einen, weil Automat vollständig ist
            i = pre_infinite_states d
        q <- eins $ setToList i
        let ws = some_shortest $ d { finals = mkSet [ q ] }
        -- gibt wenigstens eins, weil Sprache unendlich ist
        w <- eins ws
        return $ I.Instance
            { I.language = Convert.Language.Language 
                  { Convert.Language.implementation =
                       Convert.Input.NFA a     
                  , Convert.Language.description = Nothing
                  }
            , I.goal = w
            , I.wanted = C.wanted conf
            , I.minimal_length = 2 * length w
            }

instance Project
        Nerode_Congruent ( I.Instance c ) ( I.Instance c ) where
    project p x = x

make :: Make
make = quiz Nerode_Congruent C.example
