{-# LANGUAGE TemplateHaskell #-}

module NFA.Compress.Compressed where

import NFA.Compress.Data

import qualified Challenger as C

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Data.Typeable
import Data.List ( nub )

-- | compressed transition table representation
-- as described in the dragon book
data Compressed = 
     Compressed { dflt :: [ Int ]
		, base :: [ Int ]
		, chck :: [ Int ]
		, next :: [ Int ]
		}
  deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Compressed])

example :: Compressed
example = Compressed
        { dflt = [ 0 ]
	, base = [ 0 ]
	, chck = [ 0, 0, 0, 0 ]
	, next = [ 1, 2, 3, 4 ]
	}

instance C.Verify DFA_Compress Compressed where
    verify p c = do
        assert ( length ( dflt c ) == length ( base c ) )
	       $ text "default und base sollen gleichlang sein."
	assert ( length ( chck c ) == length ( next c ) )
	       $ text "check und next sollen gleichlang sein."

instance Size Compressed where 
    size c = length $ next c
	

-- local variables:
-- mode: haskell
-- end:
