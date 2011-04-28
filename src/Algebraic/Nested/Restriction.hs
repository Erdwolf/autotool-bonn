{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses #-} 

module Algebraic.Nested.Restriction where

import Algebraic.Nested.Type

import Condition

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Depth

import Autolib.Reporter

import Data.Typeable
import Data.Ix

data Restriction
    = Size_Range (Int, Int)
    | Depth_Range (Int, Int)
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Restriction])

instance ( Ord a, ToDoc a ) 
         => Condition Restriction ( Type a ) where
    condition r s = case r of
        Size_Range rng ->
	    assert ( inRange rng $ size s )
		   $ text "Größe im erlaubten Bereich" <+> toDoc rng
	Depth_Range rng -> 
	    assert ( inRange rng $ depth s )
		   $ text "Tiefe im erlaubten Bereich" <+> toDoc rng
	    

-- local variables:
-- mode: haskell
-- end:
