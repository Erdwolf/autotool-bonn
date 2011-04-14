--  -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module NFA.AI where

import Autolib.NFA.Example
import Autolib.NFA.Type (NFA)
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data AI = AI { name :: String -- abkürzung
	     , automat :: NFA Char Int   
	     }
    deriving ( Typeable )

example :: AI
example = 
    let sigma = mkSet "abc"
    in  AI { name = "irgendeine Sprache"
	   , automat = example_sigma sigma
	   }

$(derives [makeReader, makeToDoc] [''AI])

instance  Show AI where show = render . toDoc
instance  Read AI where readsPrec = parsec_readsPrec