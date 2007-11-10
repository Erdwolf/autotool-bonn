{-# OPTIONS -fglasgow-exts -XFlexibleInstances #-}

module Specify.Definition where

import Specify.Expression

import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

data Program = Program [ Definition ] 
    deriving Typeable


instance ToDoc Program where
    toDoc ( Program ds ) = vcat $ map toDoc ds

instance Reader Program where
    reader = do
        ds <- many reader
	return $ Program ds

find :: Program -> Identifier -> Reporter Definition
find ( Program ds ) name = case filter ( \ ( Definition n _ _ ) -> n == name ) ds of
    d : _ -> return d
    []    -> reject $ text "keine Definition für" <+> toDoc name

extend :: Program -> [ ( Identifier, Integer ) ] -> Program
extend ( Program ds ) pairs = 
    Program $ map ( \ (i,v) -> Definition i [] ( Constant v ) ) pairs ++ ds


data Definition = Definition Identifier [ Identifier ] ( Expression Integer ) 
    deriving Typeable

instance ToDoc Definition where
    toDoc ( Definition fun args body ) = 
        hsep [ toDoc fun, parens $ Autolib.ToDoc.sepBy comma $ map toDoc args 
	     , text "="
	     , toDoc body
	     , semi
	     ]

instance Reader Definition where
    reader = do
        fun <- ident
	args <- my_parens $ Autolib.Reader.sepBy ident my_comma
	my_symbol "="
	body <- reader
	my_symbol ";"
	return $ Definition fun args body
