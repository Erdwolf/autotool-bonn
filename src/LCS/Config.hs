{-# OPTIONS -fallow-overlapping-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module LCS.Config where

import LCS.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable

data ( ToDoc [a], Reader [a], Ord a ) => Config a =
     Config { alphabet :: Set a
	    , solution_length_min :: Int
	    , solution_length_max :: Int
	    , exactly :: Bool
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Char
example = Config
	{ alphabet            = mkSet "abcde"
	, solution_length_min = 7
	, solution_length_max = 10
        , exactly = True
	}

-- Local Variables:
-- mode: haskell
-- End:
