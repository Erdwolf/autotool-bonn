{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module NFA.Nerode.Incongruent.Config where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable

data Ord c => Config c = Config
              { alphabet :: Set c
              , nondet_automaton_size :: Int
              , wanted :: Int
              }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Char
example = Config
        { alphabet = mkSet "ab"
        , nondet_automaton_size = 5
        , wanted = 4
        }

-- local variables:
-- mode: haskell
-- end:
