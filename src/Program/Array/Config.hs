{-# OPTIONS -fglasgow-exts #-}

module Program.Array.Config where

import Program.Array.Operator

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Config =
     Config { variables_per_depth :: [ Int ]
	    , max_data_size :: Int
	    , max_expression_size :: Int
	    , statements :: Int
	    , operators :: [ Operator ]
	    }
    deriving ( Typeable )

example :: Config
example = Config
	{ variables_per_depth = [ 0, 2 ]
	, max_data_size = 5
	, max_expression_size = 6
	, statements = 5
	, operators = [ Add, Subtract, Multiply ]
	}

{-! for Config derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
