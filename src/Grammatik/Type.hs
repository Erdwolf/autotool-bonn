{-# LANGUAGE TemplateHaskell #-}

module Grammatik.Type 

( module Grammatik.Type
, module Autolib.Set
)

where


import Autolib.Set
import Autolib.Size
import Autolib.Hash
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Grammatik = Grammatik
	       { terminale      :: Set Char
	       , variablen :: Set Char
	       , start	:: Char
	       , regeln		:: Set (String, String) 
	       }
         deriving ( Eq, Typeable )

example :: Grammatik
example = Grammatik
        { terminale = mkSet "ab"
        , variablen = mkSet "S"
        , start = 'S'
        , regeln = mkSet [ ("S", ""), ("S", "aSbS") ]
        }

$(derives [makeReader, makeToDoc] [''Grammatik])

instance Hash Grammatik where
    hash g = hash [ hash $ terminale g
                  , hash $ variablen g
                  , hash $ start g
                  , hash $ regeln g
                  ]

terms = setToList . terminale
vars  = setToList . variablen
rules = setToList . regeln

instance Size Grammatik where size = cardinality . regeln

-- | for compatibility:
nichtterminale = variablen
startsymbol = start

-- local variables:
-- mode: haskell
-- end:



