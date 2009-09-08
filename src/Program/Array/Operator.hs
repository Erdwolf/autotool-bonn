{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Program.Array.Operator where

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
-- {-! for Operator derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
