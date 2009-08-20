{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Rewriting.Derive.Config where

import qualified Rewriting.Roller as R

import Autolib.TES.Identifier

import Autolib.Symbol
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data ( Symbol c, Symbol v ) => Config v c =
     Config { system_roller :: R.Config v c
            , min_size :: Int
            , max_size :: Int
            , min_steps :: Int
            , max_steps :: Int
            }
    deriving ( Eq, Ord, Typeable )
    
example :: Config Identifier Identifier
example = Config
        { system_roller = R.example
        , min_size = 5
        , max_size = 12
        , min_steps = 4
        , max_steps = 7
        }

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
