{-# LANGUAGE TemplateHaskell #-}

module Robots.Quiz where

import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data RC = RC { width :: Integer -- ^ feld geht von (-w.-w) .. (w,w)
	     , num :: Int -- ^ of robots
	     , at_least :: Int -- ^ req length of solution
	     , search_width :: Int -- ^ at most that many nodes per level
	     }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''RC])

rc :: RC
rc = RC { width = 3
	, num = 5
	, at_least = 5
	, search_width = 1000
	}

-- Local Variables:
-- mode: haskell
-- End:
