{-# LANGUAGE DeriveDataTypeable, DisambiguateRecordFields, TemplateHaskell #-}

module Unify.Config where

import Unify.Instance
import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import qualified Autolib.TES.Binu as B
import Data.Typeable

data InstanceC v c => Config v c = Config
       { signature :: B.Binu c
       , variables :: [ v ]
       , term_size :: Int
       , wildcard  :: c
       , num_wildcards :: Int
       , num_candidates :: Int -- ^ will use best instance of these
       }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Identifier Identifier
example = Config
    { signature =  B.Binu
          { binary = read "[f]" -- TODO: ghc-6.8.3 bug (?) when using B.binary = 
          , unary  = read "[]"
          , nullary = read "[a]"
          }
    , variables = read "[ X,Y,Z ]"
    , wildcard  = read "undefined"
    , term_size = 10
    , num_wildcards = 3
    , num_candidates = 3000 
    }


-- local variables:
-- mode: haskell
-- end:
