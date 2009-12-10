{-# LANGUAGE TemplateHaskell #-}

module NFA.Compress.Config where

import NFA.Compress.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import qualified Challenger as C

import Data.List ( nub )
import Data.Typeable


data Config = Config
	      { letters :: Int
	      , states :: Int
              , compressed_size :: Int
              , allow_slack :: Int -- ^  add to compressed_size,
                               -- results in bound for student
	      }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

instance C.Verify DFA_Compress Config where
    verify p c = do
        assert ( 0 < letters c ) $ text "letters > 0"
        assert ( 0 < states c ) $ text "states > 0"
        assert ( 0 < compressed_size c ) $ text "compressed > 0" 
        assert ( 0 <= allow_slack c ) $ text "slack >= 0" 
        assert ( compressed_size c > letters c ) 
               $ text "compressed > letters"
        assert ( compressed_size c + allow_slack c < letters c * states c )
               $ text "compressed + slack < letters * states"


example :: Config
example = Config
	      { letters = 5
	      , states = 5
              , compressed_size = 11
              , allow_slack = 1
	      }

-- local variables:
-- mode: haskell
-- end:

