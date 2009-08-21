{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module String_Matching.KMP.Instance where

import String_Matching.Option

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Set

import Data.Typeable

data  Ord a => Instance a = 
     Instance { alphabet :: Set a
           , word :: [ Option a ] 
           , failures :: [ Option Int] 
           }
    deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

instance Ord a => Sub ( Instance a ) ( Instance a ) where
    sub i j = and
	    [ sub ( word i ) ( word j ) 
	    , sub ( failures i ) ( failures j )
	    ]

example :: Instance Identifier
example = Instance { alphabet = mkSet [ read "a", read "b" ]
                , word = read "[ ? , ? , ? , ? , ? , b , a , ? ]"
                , failures = read "[ ? , ? , 0 , ? , 1 , ? , ? , 0 ]"
                }

-- local variables:
-- mode: haskell
-- end:
