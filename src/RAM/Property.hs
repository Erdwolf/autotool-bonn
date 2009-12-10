{-# LANGUAGE TemplateHaskell #-}

module RAM.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import RAM.Builtin

data Property = Builtins [ Builtin ]
              | No_While
              | No_Loop
     deriving ( Typeable , Eq )

$(derives [makeReader, makeToDoc] [''Property])

example :: [ Property ]
example = [ Builtins [ ]
          , No_While
	  ]

-- local variables: 
-- mode: haskell
-- end
