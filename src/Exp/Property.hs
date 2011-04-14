{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}

module Exp.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable


import Autolib.Set


data ( Ord c, ToDoc [c], Reader [c] ) 
     => Property c = 
	        Max_Size Int
	      | Alphabet ( Set c )
	      | Simple -- ^ einfacher regulärer Ausdruck (plus, mal, stern)
	      | Extended -- ^ erweiterter regulärer Ausdruck (plus, minus, durch, mal, stern)
              | Max_Star_Height Int
	      | AllowedKeys (Set String) -- ^ z. B. Empty (leeres Wort), Sigma (Alphabet), All (Sigma hoch stern)
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Property])

example :: [ Property Char ]
example = [ Simple
	  , Alphabet $ mkSet "ab"
	  , AllowedKeys $ mkSet [ "Empty" ]
	  ]


