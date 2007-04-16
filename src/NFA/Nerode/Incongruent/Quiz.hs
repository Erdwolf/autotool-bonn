{-# OPTIONS -fglasgow-exts #-}

module NFA.Nerode.Incongruent.Quiz where

import qualified NFA.Nerode.Incongruent.Config as C
import qualified NFA.Nerode.Incongruent.Instance as I
import qualified NFA.Roll 

import NFA.Property
import NFA.Infinite

import qualified Convert.Language 
import qualified Convert.Input

import NFA.Nerode.Incongruent.Check

import Inter.Quiz
import Inter.Types

import Autolib.Set
import Autolib.NFA
import Autolib.Util.Zufall

instance NFAC c Int => Generator 
        Nerode_Incongruent ( C.Config c ) ( I.Instance c ) where
    generator p conf key = do
        (a, d) <- do
            a <- NFA.Roll.roll 
               [ Alphabet $ C.alphabet conf
               , Max_Size $ C.nondet_automaton_size conf
               ]
            let d = Autolib.NFA.minimize0 a
            return (a, d)
          `repeat_until` \ (a, d) -> 
              cardinality (states d ) >= C.wanted conf
        return $ I.Instance
            { I.language = Convert.Language.Language 
                  { Convert.Language.implementation =
                       Convert.Input.NFA a     
                  , Convert.Language.description = Nothing
                  }
            , I.wanted = C.wanted conf
            }

instance Project
        Nerode_Incongruent ( I.Instance c ) ( I.Instance c ) where
    project p x = x

make :: Make
make = quiz Nerode_Incongruent C.example
