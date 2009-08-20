{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module RAM.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import RAM.Builtin

data Property = Builtins [ Builtin ]
              | No_While
              | No_Loop
     deriving ( Typeable , Eq )

{-! for Property derive: Reader, ToDoc, Haskell2Xml !-}

example :: [ Property ]
example = [ Builtins [ ]
          , No_While
	  ]

-- local variables: 
-- mode: haskell
-- end
