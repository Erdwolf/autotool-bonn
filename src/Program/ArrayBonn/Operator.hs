{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Program.ArrayBonn.Operator where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Operator = Add | Subtract | Multiply | Divide
    deriving Typeable

semantics :: Operator 
	  -> ( Integer -> Integer -> Integer )
semantics op = case op of
	        Add -> (+)
		Subtract -> (-)
		Multiply -> (*)
		Divide-> div

$(derives [makeReader, makeToDoc] [''Operator])

-- local variables:
-- mode: haskell
-- end:
