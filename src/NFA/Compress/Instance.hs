{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module NFA.Compress.Instance where

import NFA.Compress.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import qualified Challenger as C

import Data.List ( nub )
import Data.Typeable


data Instance = Instance
	      { max_size :: Int
	      , original :: [ [ Int ] ]
	      }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])
-- {-! for Instance derive: ToDoc, Reader !-}

instance C.Verify DFA_Compress Instance where
    verify p i = do
        let lengths = map length $ original i
	case nub lengths of
	    [] -> reject $ text "Tabelle ist leer"
	    [l] -> return ()
	    ls -> reject $ text "Tabelle enthält verschieden lange Zeilen"
	when ( max_size i < 0 ) $  reject
	       $ text "max_size ist negativ."

example :: Instance
example = Instance
	{ max_size = 8
	, original = [ [ 1,2,3,4], [2,2,3,4], [1,3,2,4], [2,1,3,4] ]
	}

-- local variables:
-- mode: haskell
-- end:

