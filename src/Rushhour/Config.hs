{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Rushhour.Config where

import Rushhour.Data ( Rushhour )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Typeable

import qualified Challenger as C

data Config =
     Config { width :: Int
	    , height :: Int
	    , num_cars :: Int
	    , min_extension :: Int
	    , max_extension :: Int
	    , min_solution :: Int
	    , max_solution :: Int
            , max_search_width :: Int
	    }

$(derives [makeReader, makeToDoc] [''Config])

example :: Config
example = Config
        { width = 3, height = 3, num_cars = 10
        , min_extension = 2, max_extension = 2
        , min_solution = 10, max_solution = 30
        , max_search_width = 30
        }

instance C.Verify Rushhour Config where
    verify p c = do
        assert ( min_extension c > 1 ) $ text "min_extension > 1"
	assert ( max_extension c > min_extension c ) $ text "max_extension > min_extension"
        assert ( width c > max_extension c ) $ text "width > max_extension"
        assert ( height c > max_extension c ) $ text "height > max_extension"
	assert ( num_cars c > 0 ) $ text "num_cars > 0"
	assert ( min_solution c > 0) $ text "min_solution > 0"
	assert ( max_solution c > min_solution c ) $ text "max_solution > min_solution"

-- local variables:
-- mode: haskell
-- end:
