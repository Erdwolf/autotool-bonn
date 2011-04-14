{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Hilbert.Param where

import Autolib.Reader
import Autolib.ToDoc

import Boolean.Op
import Expression.Op

import Data.Typeable

import Hilbert.Env
import qualified Hilbert.Axioms

data Param = Param
	   { target :: Exp Bool
	   , axioms :: Env ( Exp Bool )
	   }
     deriving Typeable

example :: Param
example = Param
	{ target =  read "p -> (q -> p)"
	, axioms = Hilbert.Axioms.axioms
	}

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end

