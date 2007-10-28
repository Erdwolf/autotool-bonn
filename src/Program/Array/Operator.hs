{-# LINE 1 "Program/Array/Operator.hs.drift" #-}
{-# OPTIONS -fglasgow-exts #-}

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

{-! for Operator derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
instance Reader Operator where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do my_reserved "Add"
                           return (Add))
                       <|>
                       (do my_reserved "Subtract"
                           return (Subtract))
                       <|>
                       (do my_reserved "Multiply"
                           return (Multiply))
                       <|>
                       (do my_reserved "Divide"
                           return (Divide)))

instance ToDoc Operator where
    toDocPrec d (Add) = text "Add"
    toDocPrec d (Subtract) = text "Subtract"
    toDocPrec d (Multiply) = text "Multiply"
    toDocPrec d (Divide) = text "Divide"

--  Imported from other files :-
