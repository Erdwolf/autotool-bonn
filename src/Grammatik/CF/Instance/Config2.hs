{-# LANGUAGE TemplateHaskell #-}
module Grammatik.CF.Instance.Config2 

( Config (..)
, example
)

where

-- -- $Id$

import qualified Language.Sampler
import Language.Sampler ( Sampler )

import Grammatik.Property

import Autolib.Long
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config =
     Config { source :: Sampler
	    , properties :: [ Property ]
	    }
     deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: ToDoc, Reader !-}

example :: Config
example = Config
	{ source = Language.Sampler.example
        , properties = [ Kontextfrei ]
	}

-- local variables:
-- mode: haskell
-- end:
