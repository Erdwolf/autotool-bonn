{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module FP.Conf where

import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Identifier

import Data.Typeable

data Conf = Conf { typecons ::  [ (Identifier, Int) ]
		 , typevars :: [ Identifier ]
		 , type_size_bounds :: ( Int, Int)
		 , signature_size_bounds :: ( Int, Int)
		 , target_size_bounds :: ( Int, Int)
		 , solution_size_bounds :: (Int, Int)
		 }
    deriving ( Typeable )

example :: Conf
example = Conf { typecons = read "[( List, 1), ( Tuple, 0 ), ( Tuple, 2), ( Arrow, 2), (Foo, 1), (Bar, 2)]"
	    , typevars = read "[a,b]"
		 , type_size_bounds = ( 2, 6 )
		 , signature_size_bounds = ( 3, 5 )
	    , target_size_bounds = ( 5, 20 )
		 , solution_size_bounds = ( 3, 15 )
	    }

$(derives [makeReader, makeToDoc] [''Conf])


-- local variables:
-- mode: haskell
-- end:
