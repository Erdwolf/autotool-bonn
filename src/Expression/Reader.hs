
-- -- $Id$

module Expression.Reader 

( expression
)

where

import Expression.Type
import Expression.Tokens

import Char
import Parsec


--------------------------------------------------------------------------

expression :: Parser Expression
expression = do
    header <- atomic
    case header of
        Id foo ->     named foo
		  <|> positional foo
		  <|> return header 
	otherwise -> -- if number or string
		  return header 

positional :: String -> Parser Expression
positional header = do
    args <- many1 atomic
    return $ Positional header args

named :: String -> Parser Expression
named header = braces $ do 
    args <- commaSep bind
    return $ Named header args

bind :: Parser (String, Expression)
bind = do
    n <- identifier
    reservedOp "="
    v <- expression
    return (n, v)    

atomic :: Parser Expression
atomic =   fmap Num integer
       <|> fmap Strg stringLiteral
       <|> fmap Id identifier
       <|> tuple
       <|> list
       <?> "atomic"

list :: Parser Expression
list = squares $ do
     xs <- commaSep expression
     return $ List xs

tuple :: Parser Expression
tuple = parens $ do
     xs <- commaSep expression
     case xs of
	  [x] -> return x
	  otherwise -> return $ Tuple xs

--------------------------------------------------------------------------

instance Read Expression where
    readsPrec _ inp =
        case parse express "<stdin>" inp of
	     Right (x, rest) -> return (x, rest)
	     Left err -> error ("\n" ++ inp ++ "\n" ++ show err)

express :: Parser (Expression, String)
express = do 
    whiteSpace
    x <- expression
    rest <- getInput
    return (x, rest)



