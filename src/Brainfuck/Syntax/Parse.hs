module Brainfuck.Syntax.Parse 
( program
) where

import Brainfuck.Syntax.Data

import qualified Autolib.Reader

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr

brainfuck :: LanguageDef st
brainfuck = emptyDef                      
                { opLetter       = oneOf ""
                , reservedOpNames= ["+","-","<",">",".",","]
                , reservedNames  = []
                , caseSensitive  = False
                }

-- bf :: TokenParser ()
bf = makeTokenParser brainfuck


program :: Parser Statement
program = do
   sts <- Text.ParserCombinators.Parsec.Token.parens bf $ many statement
   return $ Block $ filter ( \ s -> s /= Null ) sts
   
statement :: Parser Statement
statement = loop
        <|> single


instance Autolib.Reader.Reader Statement where
    reader = program
   
single = do
    c <- noneOf "[]()"
    case c of
       '+' -> return Plus
       '-' -> return Minus
       '<' -> return MLeft
       '>' -> return MRight
       '.' -> return Output
       ',' -> return Input
       _ -> return Null
       
loop = Text.ParserCombinators.Parsec.Token.brackets bf $ do 
    sts <- many statement
    return $ Loop $ filter ( \ s -> s /= Null ) sts

