{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Grammatik.Trace.Config where

import qualified Grammatik.Type as G
import qualified Grammatik.Ableitung.Config as A

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data Config = Config
	   { grammatik :: G.Grammatik
           , steps :: Int
           , search :: A.Config
	   }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc !-}

-- | als default benutzbar
example :: Config 
example = Config
       { grammatik = G.example
       , steps = 5
       , search = A.example
       }

-- local variables:
-- mode: haskell
-- end:
