module Program.Array.Statement where

import Program.Array.Expression
import Program.Array.Value

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Size

data Statement = Assign Access Expression
	       | Declare Identifier [ Int ] Value

s0 :: Statement
s0 = read "x[x[2]] = x[x[0]+1];"

d0 :: Statement
d0 = read "int x = 8;"

d1 :: Statement
d1 = read "int [2] y = {5,3,1};"

d2 :: Statement
d2 = read "int [2][2] z = {{1,2},{3,4}};"

instance Size Statement where
    size ( Declare {} ) = 1
    size ( Assign target exp ) = size exp

instance ToDoc Statement where
    toDoc s = case s of
        Assign target exp ->
	    hsep [ toDoc target, equals, toDoc exp, semi ]
	Declare name dim val ->
            hsep [ text "int"
		 , hsep ( do d <- dim ; return $ toDoc [d] )
		 , toDoc name
	        , equals
	        , toDoc val
	        , semi
	        ]

instance Reader Statement where
    reader = declaration <|> assignment

declaration = do
        my_reserved "int"
	dims <- many $ my_brackets $ fmap fromIntegral my_integer
	name <- reader
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




