-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}

module Exp.Quiz where

import Autolib.NFA

import qualified NFA.Property as A
import qualified Exp.Property as E

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable


data NFAC c Int => 
     Quiz c = Quiz { generate :: [ A.Property c ]
		 , solve    :: [ E.Property c ]
		 }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Quiz])

example :: Quiz Char
example = Quiz { generate = [ A.Alphabet $ mkSet "ab"
			    , A.Max_Size 5
			    ]
	       , solve    = [ E.Alphabet $ mkSet "ab"
			    , E.Simple
			    ]
	       }
