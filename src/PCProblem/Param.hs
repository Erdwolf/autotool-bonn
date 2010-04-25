{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module PCProblem.Param where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Param =
     Param { alpha :: String -- ^ zu benutzende Buchstaben
	       , paare :: Int -- ^ anzahl
	       , breite :: Int -- ^ maximale wortlänge
	       , nah :: Int -- ^ keines mit kürzerer lösung
	       , fern :: Int -- ^ keines mit längerer lösung
	       , viel :: Int -- ^ nur soviele Knoten im suchbaum (bfs)
	       }
     deriving ( Typeable )

g :: Param
g = Param { alpha = "abc"
	      , paare = 4
	      , breite = 3
	      , nah = 7
	      , fern = 20
	      , viel = 1000
	      }

$(derives [makeReader, makeToDoc] [''Param])

instance Show Param where show = render . toDoc
instance Read Param where readsPrec = parsec_readsPrec

