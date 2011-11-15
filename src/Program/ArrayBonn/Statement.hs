{-# LANGUAGE DeriveDataTypeable #-}

module Program.ArrayBonn.Statement where

import Program.ArrayBonn.Expression
import Program.ArrayBonn.Value

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Size

import Data.Typeable

data Statement = Assign Access Expression
               | Declare Identifier [ Int ] Value
            deriving Typeable

s0 :: Statement
s0 = read "x[x[2]] = x[x[0]+1];"

d0 :: Statement
d0 = read "int x = 8;"

d1 :: Statement
d1 = read "int y[2] = {5,3,1};"

d2 :: Statement
d2 = read "int z[2][2] = {{1,2},{3,4}};"

instance Size Statement where
    size ( Declare {} ) = 1
    size ( Assign target exp ) = size exp

instance ToDoc Statement where
    toDoc s = case s of
        Assign target exp ->
            hsep [ toDoc target, equals, toDoc exp, semi ]
        Declare name dim val ->
            hsep [ text "int"
                 , toDoc name <> hsep ( do d <- dim ; return $ toDoc [d] )
                 , equals
                 , toDoc val
                 , semi
                 ]

instance Reader Statement where
    reader = declaration <|> assignment

declaration = do
    my_reserved "int"
    name <- reader
    dims <- many $ my_brackets $ fmap fromIntegral my_integer
    my_equals
    val <- value dims
    my_semi
    return $ Declare name dims val

assignment = do
    target <- reader
    my_equals
    exp <- reader
    my_semi
    return $ Assign target exp
