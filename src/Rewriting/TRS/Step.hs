{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module Rewriting.TRS.Step where

import Rewriting.TRS

import Autolib.Size
import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap

import Data.Typeable

data ( Ord v, Symbol c ) => Step v c = Step
          { rule_number :: Int
          , position :: Position
          , substitution :: FiniteMap v ( Term v c )
          }
    deriving ( Eq, Ord, Typeable )

instance Size ( Step v c ) where size _ = 1

$(derives [makeReader, makeToDoc] [''Step])

-- local variables:
-- mode: haskell
-- end:
