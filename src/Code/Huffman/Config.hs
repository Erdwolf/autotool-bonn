{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts #-} 

module Code.Huffman.Config where

--  $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set
import Data.Typeable

data ( Reader [a], ToDoc [a], Ord a ) => Config a = 
     Config { alphabet :: Set a
	    , range    :: (Int, Int)
	    }
     deriving ( Eq, Ord, Typeable )

example :: Config Char
example = Config { alphabet = mkSet [ 'a' .. 'f' ]
                 , range    = ( 5, 50 )
                 }

$(derives [makeReader, makeToDoc] [''Config])

-- local variables:
-- mode: haskell
-- end;
