{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module String_Matching.Config where

import Autolib.TES.Identifier

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Ord  a => Config a =
     Config { alphabet :: Set a
            , word_length :: Int
            , take_best_of :: Int
            }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Identifier
example = Config
        { alphabet = mkSet [ read "a", read "b" ]
        , word_length = 12
        , take_best_of = 5
        }

-- local variables:
-- mode: haskell
-- end:
