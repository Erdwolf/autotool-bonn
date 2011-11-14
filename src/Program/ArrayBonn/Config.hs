{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Program.ArrayBonn.Config where

import Program.ArrayBonn.Operator

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Config =
     Config { variables_per_depth :: [ Int ]
	    , max_data_size :: Int
	    , max_expression_size :: Int
	    , statements :: Int
	    , operators :: [ Operator ]
        , seed :: Int -- Dummy value
	    }
    deriving ( Typeable )

example :: Config
example = Config
	{ variables_per_depth = [ 0, 2 ]
	, max_data_size = 5
	, max_expression_size = 6
	, statements = 5
	, operators = [ Add, Subtract, Multiply ]
    , seed = 0
	}

$(derives [makeReader, makeToDoc] [''Config])

-- local variables:
-- mode: haskell
-- end:
