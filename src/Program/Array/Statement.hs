module Program.Array.Statement where

import Program.Array.Expression
import Program.Array.Value

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier

data Statement = Assign Access Expression
	       | Declare Identifier Int Value

example :: Statement
example = read "x[x[2]] = y[(a + 3)*5][4];"

instance ToDoc Statement where
    toDoc s = case s of
        Assign target exp ->
	    hsep [ toDoc target, equals, toDoc exp, semi ]
	Declare name dim val ->
            hsep [ text "int", hsep ( replicate dim $ text "[]" )
	        , equals
	        , toDoc val
	        , semi
	        ]

instance Reader Statement where
    reader = declaration <|> assignment

declaration = do
        my_reserved "int"
	name <- reader
	ds <- many $ my_brackets $ return ()
	my_equals
	val <- reader
	my_semi
	return $ Declare name ( length ds ) val

assignment = do
        target <- reader
	my_equals
	exp <- reader
	my_semi
	return $ Assign target exp




