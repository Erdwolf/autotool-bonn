module FP.Type where

import FP.Expression
import FP.Arrow

import Autolib.TES.Identifier
import Autolib.TES.Position

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Autolib.Size

import Data.Char


data Type = 
     Type { prefix :: [ Identifier ]
	  , core :: Arrow Identifier
	  }

instance Size Type where size = size . core

instance Eq Type where s == t = core s == core t
instance Ord Type where compare s t = compare ( core s ) ( core t )

instance ToDoc Type where
    toDoc t = case prefix t of
         [] -> toDoc $ core t
         ps -> fsep 
	    [ text "forall"
	    , hsep $ map toDoc ps
	    , text "."
	    , toDoc $ core t
	    ]

instance Reader Type where
    reader = do
        ps <- option []  pre
	c <- parse_arrow ps
	return $ Type { prefix = ps , core = c }

wrap :: Arrow Identifier -> Type
wrap t = Type { prefix = setToList $ vars $ unArrow t , core = t }

pre :: Parser [ Identifier ]
pre = do
    my_reserved "forall"
    ps <- many name
    my_reserved "."
    return ps



    

