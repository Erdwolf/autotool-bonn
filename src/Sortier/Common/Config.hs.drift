module Sortier.Common.Config where

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data Config =
     Config { width :: Int
	    , max_size :: Int
	    }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc !-}

example :: Config
example = Config { width = 6, max_size = 13 }

-- local variables:
-- mode: haskell
-- end: