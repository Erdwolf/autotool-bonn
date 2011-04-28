{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts #-} 

module NFA.Nerode.Congruent.Instance where

import Autolib.Reader
import Autolib.ToDoc


import Convert.Language

import Autolib.Exp
import Autolib.NFA ( NFAC )

import Data.Typeable


data NFAC c Int => Instance c =
     Instance { language :: Language c
              , goal :: [c]
              , wanted :: Int
              , minimal_length :: Int
              }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

example :: Instance Char
example = Instance
        { language = Convert.Language.example
        , goal = "ab"
        , wanted = 4
        , minimal_length = 6
        }

-- local variables:
-- mode: haskell
-- end;
