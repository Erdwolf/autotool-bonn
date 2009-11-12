{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Fun.Direct.Config (
    module Fun.Direct.Config,
    module Fun.Matrix,
    mktafel2,
    mkmatrix
) where

import Fun.Type
import Fun.Matrix
import Fun.Table

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Typeable
import Data.Array
import Data.List

data Primrec_2D = Primrec_2D deriving ( Typeable )

data Config = 
     Config { table :: Matrix Integer
	    , properties :: [ Property ]
	    }
    deriving Typeable

example :: Config
example = Config
    { Fun.Direct.Config.table = examplematrix 10
    , properties = [ Builtins [] ]
    }

$(derives [makeReader, makeToDoc] [''Primrec_2D])

$(derives [makeReader, makeToDoc] [''Config])

-- local variables:
-- mode: haskell
-- end:
