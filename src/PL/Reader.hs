{-# language PatternSignatures #-}

module PL.Reader where

import PL.Data
import PL.ToDoc

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
		 AssocRight ]
       , [ Infix ( do my_reserved "||" ; return $ \ x y -> Operation Or  [x,y] ) 
		 AssocRight ]
       , [ Infix ( do my_reserved "<=>" ; return $ \ x y -> Operation Iff [x,y] ) 
		 AssocNone	  
	 , Infix ( do my_reserved "=>" ; return $ \ x y -> Operation Implies [x,y] ) 
		 AssocRight	  
	 ]
       ]
       atomic

atomic :: Parser Formel
atomic = quantified <|> predicate <|> my_parens reader

identifier :: Parser Identifier
identifier = do
    x <- letter
    xs <- many ( letter <|> digit )
    my_whiteSpace
    return $ mknullary $ x : xs

instance Reader Quantor where
    reader = do my_reserved "forall" ; return Forall
         <|> do my_reserved "exists" ; return Exists
	 <|> do my_reserved "count"  ; c <- reader ; n <- reader
	        return $ Count c n

instance Reader Compare where
    reader = do
	c <- foldr1 (<|>) $ do
            c <- [ minBound .. maxBound ]
	    return $ do try $ my_symbol ( show c ) ; return c 
        my_whiteSpace
	return c

quantified :: Parser Formel
quantified = do
    q :: Quantor <- reader
    vs :: [ Identifier ] <- many1 identifier
    my_reserved "."
    f :: Formel <- reader
    return $ foldr ( Quantified q ) f vs 

-- | the atomic parser for predicate symbol with arguments,
-- or term equals term.
-- Beispiel 1: P(x,y), Beispiel 2: f(x,y) == g(z)
-- das kann der Parser erst beim "==" unterscheiden. 
-- noch schlimmer: Beispiel 3:  x == f(y),
-- weil das x keine Klammern hat
predicate :: Parser Formel
predicate = do
    ( p :: Identifier ) <- identifier
    args <- option Nothing $ do
        xs <- my_parens $ reader `Autolib.Reader.sepBy` my_comma
	return $ Just xs
    case args of
        Nothing -> equality ( Variable p )
	Just xs -> option ( Predicate p xs ) $ equality ( Apply p xs )

equality lhs = do
    try $ my_symbol "=="
    rhs :: Term <- reader
    return $ Equals lhs rhs

instance Reader Term where
    reader = do
        ( f :: Identifier ) <- identifier
	args <- option Nothing $ do
	    xs <- my_parens $ reader `Autolib.Reader.sepBy` my_comma
	    return $ Just xs
	case args of
	    Nothing -> return $ Variable f
	    Just xs -> return $ Apply f xs

