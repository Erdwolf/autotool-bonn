{-# LANGUAGE TemplateHaskell #-}

module NFA.Nerode.Incongruent.Instance where

import Autolib.Reader
import Autolib.ToDoc

import Convert.Language

import Autolib.Exp
import Autolib.NFA ( NFAC )

import Data.Typeable

data NFAC c Int => Instance c =
     Instance { language :: Language c
              , wanted :: Int
              }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

example :: Instance Char
example = Instance
        { language = Convert.Language.example
        , wanted = 4
        }

-- local variables:
-- mode: haskell
-- end;
