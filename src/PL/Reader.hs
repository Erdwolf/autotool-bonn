{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module PL.Reader where

import PL.Data

import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Identifier

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec

instance Reader Formel where
    reader = buildExpressionParser 
       [ [ Prefix ( do my_reserved "not" ; return $ \ x -> Operation Not [x] ) 
	 ]
       , [ Infix ( do my_reserved "&&" ; return $ \ x y -> Operation And [x,y] )
		 AssocLeft ]
       , [ Infix ( do my_reserved "||" ; return $ \ x y -> Operation Or  [x,y] ) 
		 AssocLeft ]
       , [ Infix ( do my_reserved "<=>" ; return $ \ x y -> Operation Iff [x,y] ) 
		 AssocNone	  
	 , Infix ( do my_reserved "=>" ; return $ \ x y -> Operation Implies [x,y] ) 
		 AssocNone	  
	 ]
       ]
       atomic

atomic :: Parser Formel
atomic = quantified <|> predicate <|> my_parens reader

instance Reader Quantor where
    reader = do my_reserved "forall" ; return Forall
         <|> do my_reserved "exists" ; return Exists

quantified :: Parser Formel
quantified = do
    ( q :: Quantor ) <- reader
    ( v :: Identifier ) <- reader
    my_reserved "."
    ( f :: Formel ) <- reader
    return $ Quantified q v f


predicate :: Parser Formel
predicate = do
    ( p :: Identifier ) <- reader
    args <- my_parens $ reader `Autolib.Reader.sepBy` my_comma
    return $ Predicate p args

instance Reader Term where
    reader = do
        ( f :: Identifier ) <- reader
	args <- option Nothing $ do
	    xs <- my_parens $ reader `Autolib.Reader.sepBy` my_comma
	    return $ Just xs
	case args of
	    Nothing -> return $ Variable f
	    Just xs -> return $ Apply f xs

