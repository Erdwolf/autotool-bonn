{-# LANGUAGE TemplateHaskell #-}

module Exp.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable


import Autolib.Set


data ( Ord c, ToDoc [c], Reader [c] ) 
     => Property c = Min_Size Int
	      | Max_Size Int
	      | Alphabet ( Set c )
	      | Simple
	      | Extended
	      | AllowedKeys (Set String)
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Property])

example :: [ Property Char ]
example = [ Simple
	  , Alphabet $ mkSet "ab"
	  , AllowedKeys $ mkSet [ "Empty" ]
	  ]

-- local variables;
-- mode: haskell;
-- end;
