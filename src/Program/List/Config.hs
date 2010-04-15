{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Program.List.Config where

import Program.List.Operation
import qualified Program.List.Value as V

import Autolib.Reader
import Autolib.TES.Identifier
import Autolib.ToDoc

import Data.Typeable

data Declaration = Declaration V.Type Identifier 
    deriving ( Typeable, Eq )

instance ToDoc Declaration where
    toDoc ( Declaration ty na ) = toDoc ty <+> toDoc na
instance Reader Declaration where
    reader = do
        ty <- reader
        na <- reader
        return $ Declaration ty na

data Config =
     Config { variables :: [ Declaration ]
	    , data_size_bounds :: (Int,Int)
	    , expression_depth_bounds :: (Int,Int)
	    , program_length_bounds :: (Int,Int)
	    }
    deriving ( Typeable, Eq )

example :: Config
example = Config
	{ variables = read "[ List<int> x, List<int> y ]"
	, data_size_bounds = (5,5)
	, expression_depth_bounds = (5,7)
	, program_length_bounds = (5,5)
	}

$(derives [makeReader, makeToDoc] [''Config])

