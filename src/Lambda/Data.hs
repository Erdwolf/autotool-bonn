{-# OPTIONS -fglasgow-exts #-}

module Lambda.Data where

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Text.ParserCombinators.Parsec ( parse )

data Lambda 
    = Variable Identifier
    | Apply Lambda Lambda
    | Abstract Identifier Lambda
    deriving ( Eq, Ord )


applications :: Lambda -> ( Lambda, [ Lambda ] )
applications t = case t of
    Apply f x -> 
        let ( g, args ) = applications f
        in  ( g, args ++ [x] )
    _ -> ( t, [] )

apply :: ( Lambda, [ Lambda ] ) -> Lambda
apply ( f , args ) = foldl Apply f args

abstractions :: Lambda -> ( [ Identifier ] , Lambda )
abstractions t = case t of
    Abstract x b -> 
        let ( vars, c ) = abstractions b
        in  ( x : vars , c )
    _ -> ( [], t )

abstract :: ( [ Identifier ] , Lambda ) -> Lambda
abstract ( vars, body ) = foldr Abstract body vars

instance ToDoc Lambda where
    toDocPrec p t = case t of
        Variable v -> toDoc v
        Apply {} -> 
            let ( fun, args ) = applications t
            in docParen ( p > 0 ) 
                   $ toDoc fun <+> fsep ( map ( toDocPrec 9 ) args )
        Abstract {} -> 
            let ( vars, body ) = abstractions t
            in docParen ( p > 0 ) 
                   $ fsep ( map toDoc vars )
                   <+> text "->"
                   <+> toDocPrec 0 body

instance Reader Lambda where
    reader = application

atomic :: Parser Lambda 
atomic = my_parens ( reader :: Parser Lambda )
     <|> try abstraction
     <|> do this :: Identifier <- reader ; return $ Variable this

abstraction :: Parser Lambda
abstraction = do
    vars <- many ( reader :: Parser Identifier )
    my_reserved "->"
    body <- reader
    return $ abstract ( vars , body )

application :: Parser Lambda
application = do
    f : args <- many1 ( atomic :: Parser Lambda )
    return $ apply ( f , args )


        
