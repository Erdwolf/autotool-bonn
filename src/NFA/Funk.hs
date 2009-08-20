{-# OPTIONS -fallow-overlapping-instances #-}

-- | expressions that describe linear family
-- of automata

{-# LANGUAGE TemplateHaskell #-}
module NFA.Funk where

import Autolib.ToDoc
import Autolib.Reader

data Entry = Relative Int
	  | Absolute Int
     deriving ( Eq, Ord ) 

data Funk = Sizecase { fixed :: [(Int,Int)] 
		     , linear :: (Entry, Entry)
		     }
     deriving ( Eq, Ord )

ex :: Funk
ex = Sizecase { fixed = [(1, 1)]
	      , linear = (Relative 1, Relative 0)
	      }

$(derives [makeReader, makeToDoc] [''Entry])
-- {-! for Entry derive: ToDoc, Reader !-} 
$(derives [makeReader, makeToDoc] [''Funk])
-- {-! for Funk derive: ToDoc, Reader !-} 

instance Show Entry where show = render . toDoc
instance Show Funk  where show = render . toDoc
