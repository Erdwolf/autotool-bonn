-- -*- mode: haskell -*-

module NFA.Equiv.Conf where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Conf = Conf { alphabet :: Set Char
		 , nfa_size :: Int
		 , min_dfa_size :: Int
		 , max_dfa_size :: Int
		 }
     deriving ( Eq, Ord, Typeable )

{-! for Conf derive: Reader, ToDoc, Haskell2Xml !-}

example :: Conf
example = Conf { alphabet = mkSet "ab"
	       , nfa_size = 7
	       , min_dfa_size = 9
	       , max_dfa_size = 13
	       }


