module Program.Array.Declare where

import Program.Array.Value

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc

-- | declaration with initialization
data Declare = Declare Identifier Int Value

instance ToDoc Declare where
    toDoc ( Declare name dim val ) = 
        hsep [ text "int", hsep ( replicate dim $ text "[]" )
	     , equals
	     , toDoc val
	     , semi
	     ]

instance Reader Declare where
    reader = do
        my_reserved "int"
	name <- reader
	ds <- many $ my_brackets $ return ()
	my_equals
	val <- reader
	my_semi
	return $ Declare name ( length ds ) val
