-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module NFA.Equiv.Conf where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Data.Typeable

data Conf = Conf { alphabet :: Set Char
		 , nfa_size :: Int
		 , min_dfa_size :: Int
		 , max_dfa_size :: Int
		 }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Conf])

example :: Conf
example = Conf { alphabet = mkSet "ab"
	       , nfa_size = 7
	       , min_dfa_size = 9
	       , max_dfa_size = 13
	       }


