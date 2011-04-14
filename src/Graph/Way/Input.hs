-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

--  $Id$

module Graph.Way.Input where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable ( Typeable )

data Solvability = Solvable 
		 | Unsolvable 
		   deriving ( Eq , Ord , Show , Read , Typeable )

$(derives [makeReader, makeToDoc] [''Solvability])

data Input = Input { matrix      :: [[Integer]]
		   , solvability :: Solvability
		   }
	     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Input])

ex :: Input
ex = Input { matrix      = [[1,1,0],[1,1,0],[0,0,1]]
	   , solvability = Solvable
	   }
