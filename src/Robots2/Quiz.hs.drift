{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots2.Quiz where

import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data RC = RC { width :: Integer -- ^ feld geht von (-w.-w) .. (w,w)
	     , num_robots :: Int 
	     , num_targets :: Int 
	     , at_least :: Int -- ^ req length of solution
	     , search_width :: Int -- ^ at most that many nodes per level
	     }
     deriving ( Typeable )

{-! for RC derive: Reader, ToDoc, Haskell2Xml !-}

rc :: RC
rc = RC { width = 3
	, num_robots = 5
	, num_targets = 2
	, at_least = 5
	, search_width = 1000
	}

-- Local Variables:
-- mode: haskell
-- End:
