{-# LANGUAGE DeriveDataTypeable #-}
module Specify.Definition where

import Specify.Expression

import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Reporter

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

data Program = Program [ Definition ] 
    deriving Typeable


example :: Program
example = read "{ f (x) = if 0 == x % 2 then ? else 3*x + 1; }"

instance ToDoc Program where
    toDoc ( Program ds ) = braces $ vcat $ map toDoc ds

instance Reader Program where
    reader = my_braces $ do
        ds <- many reader
	return $ Program ds

instance Size Program where
    size ( Program ds ) = sum $ map size ds

find :: Program -> Identifier -> Reporter Definition
find ( Program ds ) name = case filter ( \ ( Definition n _ _ ) -> n == name ) ds of
    d : _ -> return d
    []    -> reject $ text "keine Definition für" <+> toDoc name

extend :: Program -> [ ( Identifier, Integer ) ] -> Program
extend ( Program ds ) pairs = 
    Program $ map ( \ (i,v) -> Definition i [] ( Constant v ) ) pairs ++ ds

make :: [ ( Identifier, Maybe Integer ) ] -> Program
make pairs =  
    Program $ map ( \ (i,v) -> Definition i [] $ einpack v ) pairs

einpack v = case v of 
            Nothing -> Undefined
            Just v  -> Constant v

data Definition = Definition Identifier [ Identifier ] ( Expression Integer ) 
    deriving Typeable

instance Size Definition where
    size ( Definition _ _ x ) = size x

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
