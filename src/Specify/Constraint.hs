{-# OPTIONS -fglasgow-exts -XFlexibleInstances #-}

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
    toDoc ( System cs ) = vcat $ map toDoc cs

instance Reader System where
    reader = do
        cs <- many reader
	return $ System cs


data Constraint = Constraint [ Identifier ] ( Expression Bool ) 
    deriving Typeable

instance ToDoc Constraint where
    toDoc ( Constraint [] body ) = toDoc body <+> semi
    toDoc ( Constraint vs body ) = 
        hsep [ text "forall", hsep $ map toDoc vs
	     , text "." , toDoc body , semi
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
