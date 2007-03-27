module FP.Type where

import FP.Expression
import FP.Arrow

import Autolib.TES.Identifier
import Autolib.TES.Position

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Char


data Type = 
     Type { prefix :: [ Identifier ]
	  , core :: Arrow Identifier
	  }

instance ToDoc Type where
    toDoc t = fsep 
	    [ text "forall"
	    , hsep $ map toDoc $ prefix t
	    , text "."
	    , toDoc $ core t
	    ]

instance Reader Type where
    reader = do
        ps <- {- option [] -}  pre
	c <- parse_arrow ps
	return $ Type { prefix = ps , core = c }

wrap :: Arrow Identifier -> Type
wrap t = Type { prefix = setToList $ vars $ unArrow t , core = t }

pre :: Parser [ Identifier ]
pre = do
    my_reserved "forall"
    ps <- many1 name
    my_reserved "."
    return ps



    

