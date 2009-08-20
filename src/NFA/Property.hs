{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module NFA.Property where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import Autolib.Set
import Autolib.NFA


data NFAC c Int => Property c 
              = Sane
	      | Min_Size Int 
	      | Max_Size Int 

	      | Alphabet ( Set c )
	      | Deterministic
	      | Non_Deterministic
	      | Minimal
	      | Complete
	      | Reduced -- no useless states
     deriving ( Typeable , Eq )

{-! for Property derive: Reader, ToDoc, Haskell2Xml !-}

example :: [ Property Char ]
example = [ Sane
	  , Min_Size 4 , Max_Size 6
	  , Alphabet $ mkSet "ab"
	  ]

-- local variables;
-- mode: haskell;
-- end;
