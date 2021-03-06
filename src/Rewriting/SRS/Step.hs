{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Rewriting.SRS.Step where

-- import Rewriting.SRS

import Autolib.Size
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Symbol

import Data.Typeable

data ( Symbol c ) => Step c = Step
          { rule_number :: Int
          , position :: Int
          }
    deriving ( Eq, Ord, Typeable )

instance Size ( Step c ) where size _ = 1

$(derives [makeReader, makeToDoc] [''Step])

-- local variables:
-- mode: haskell
-- end:
