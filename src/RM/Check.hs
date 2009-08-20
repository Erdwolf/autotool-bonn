{-# LANGUAGE TemplateHaskell #-}
module RM.Check where

import RM.Type

import Machine.Numerical.Config

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Checker = MaxReg Register
	     | NumReg Int
	     | UseReg (Set Register)
	       deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Checker])
-- {-! for Checker derive: Reader, ToDoc, Haskell2Xml !-}

instance Check Checker Program where
    check (MaxReg r) p = do
        assert ( max_reg p <= r ) $ 
            text $ concat [ "Benutzen Sie höchstens das "
			  , show r
			  , "-te Register?"
			  ]
    check (NumReg k) p = do
        assert ( cardinality (regs p) <= k ) $ 
            text $ concat [ "Benutzen Sie höchstens "
			  , show k
			  , " Register?"
			  ]
    check (UseReg rs) p = do
        assert ( subseteq (regs p) rs ) $ 
            text $ concat [ "Benutzen Sie höchstens die Register "
			  , show rs
			  , "?"
			  ]
