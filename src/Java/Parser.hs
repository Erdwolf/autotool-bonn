module Java.Parser where

-- $Id$

import Java.Type

import Char

import Parsec
import ParsecExpr
import ParsecLanguage
import ParsecToken

import Reader


instance Reader Java where
    readerPrec _ = java

instance Read Java where
    readsPrec = parsec_readsPrec

me = makeTokenParser javaStyle


java :: Parser Java
java = do
    sts <- many statement
    return $ Java sts

statement :: Parser Statement
statement =   block
	  <|> while
	  <|> iff
 	  <|> declare_int
	  <|> Java.Parser.exp
	  <|> ret
	  <?> "statement"

block :: Parser Statement
block = do
    ss <- braces me $ many statement
    return $ Block ss

while :: Parser Statement
while = do
    reserved me "while"
    e <- parens me expression
    s <- block
    return $ While e s

iff :: Parser Statement
iff = do
    reserved me "if"
    e <- parens me expression
    y <- block
    reserved me "else"
    n <- block
    return $ If e y n

exp :: Parser Statement
exp = do -- mostly, assignment
    e <- expression
    semi me
    return $ Exp e

declare_int :: Parser Statement
declare_int = do
    reserved me "int" -- TODO: generalize
    e <- expression
    semi me
    return $ Declare_Int e

ret :: Parser Statement
ret = do
    reserved me "return"
    e <- expression
    semi me
    return $ Return e

expression = buildExpressionParser operators atomic

atomic = do { n <- natural me ; return $ Number n }
     <|> do { n <- identifier me ; return $ Name n }
     <|> do { e <- parens me expression ; return $ Parens e }
     <?> "atomic expression"
	    
operators = 
    [ [ op "*"  AssocLeft
      , op "/"    AssocLeft
      , op "%"    AssocLeft
      ]	       
    , [ op "+"    AssocLeft
      , op "-"    AssocLeft
      ]	       
    , [ op "<"  AssocLeft -- wirklich?
      , op ">"   AssocLeft
      , op "<="  AssocLeft
      , op ">="  AssocLeft
      , op "!="  AssocLeft
      , op "=="  AssocLeft
      ]
    , [ op "="  AssocLeft
      , op "+="  AssocLeft
      , op "-="  AssocLeft
      , op "*="  AssocLeft
      , op "/="  AssocLeft
      , op "%="  AssocLeft
      ]
    ]
    where
      op name assoc   =
         Infix ( do { reservedOp me name; return $ Operation name }
                 <?> "operator" ) assoc
