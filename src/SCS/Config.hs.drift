{-# OPTIONS -fallow-overlapping-instances #-}
{-# language FlexibleContexts, UndecidableInstances #-}
{-# language DeriveDataTypeable #-}

module SCS.Config where

import SCS.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data ( ToDoc [a], Reader [a], Ord a ) => Config a =
     Config { alphabet :: Set a
            , length_solution :: Int
            , length_subwords :: Int
	    , num_subwords :: Int
	    }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

example :: Config Char
example = Config
	{ alphabet            = mkSet "abc"
	, length_solution = 10
	, length_subwords = 5
        , num_subwords = 3
	}

-- Local Variables:
-- mode: haskell
-- End:
