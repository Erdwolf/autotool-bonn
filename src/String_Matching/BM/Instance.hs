{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses #-} 

module String_Matching.BM.Instance where

import String_Matching.Option

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Set

import Data.Typeable

data  Ord a => Instance a = 
     Instance { alphabet :: [ a ] 
                -- ^ as List because we need ordering for lambda
           , word :: [ Option a ] 
           , bad_character :: [ Option Int] 
	   , good_suffix :: [ Option Int ]
	     -- ^ this is m - \gamma (as in CLR)
           }
    deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

instance Ord a => Sub ( Instance a ) ( Instance a ) where
    sub i j = and
	    [ sub ( word i ) ( word j ) 
	    , sub ( bad_character i ) ( bad_character j )
	    , sub ( good_suffix i ) ( good_suffix j )
	    ]


example :: Instance Identifier
example = Instance 
	{ alphabet = [ read "a", read "b", read "c" ]
        , word = read "[ ? , ? , ? , ? , ? , b , a , ? ]"
        , bad_character = read "[ ? , ? , 0 ]"
        , good_suffix = read "[ ? , ? , 0 , ? , 1 , ? , ? , 0 ]"
        }

-- local variables:
-- mode: haskell
-- end:
