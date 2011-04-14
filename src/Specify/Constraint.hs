{-# LANGUAGE DeriveDataTypeable #-}
module Specify.Constraint where

import Specify.Expression

import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

data System = System [ Constraint ] 
    deriving Typeable

instance ToDoc System where
    toDoc ( System cs ) = braces $ vcat $ map toDoc cs

instance Reader System where
    reader = my_braces $ do
        cs <- many reader
	return $ System cs

example :: System
example = read "{ forall x .  f (x + 1) == 2 + f(x) ; }" 

data Constraint = Constraint [ Identifier ] ( Expression Bool ) 
    deriving Typeable

instance ToDoc Constraint where
    toDoc ( Constraint [] body ) = toDoc body <+> semi
    toDoc ( Constraint vs body ) = 
        vcat [ hsep [ text "forall", hsep $ map toDoc vs , text "." ]
	     , toDoc body , semi
	     ]

instance Reader Constraint where
    reader = do
        vs <- option [] $ do
	    my_reserved "forall"
	    vs <- many ident
	    my_symbol "."
	    return vs
	body <- reader
	my_symbol ";"
	return $ Constraint vs body
