{-# LANGUAGE DeriveDataTypeable, DisambiguateRecordFields, TemplateHaskell #-}

module Unify.Config where

import Unify.Instance
import Prolog.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Set

import qualified Autolib.TES.Binu as B
import Data.Typeable

-- | maps Identifier to its arity
type Signature = FiniteMap Identifier Int

data  Config = Config
       { signature :: Signature
       , variables :: Set Identifier 
       , term_size :: Int
       , wildcard  :: Identifier
       , num_wildcards :: Int
       , num_candidates :: Int -- ^ will use best instance of these
       }
    deriving ( Typeable, Eq )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config 
example = Config
    { signature = listToFM
          $ read "[ (f,2),(a,0) ]"
    , variables = mkSet $ read "[X,Y,Z]"
    , wildcard  = read "undefined"
    , term_size = 10
    , num_wildcards = 3
    , num_candidates = 3000 
    }



