-- -*- mode: haskell -*-

--  $Id$

module Graph.Way.Input where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable ( Typeable )
import Text.XML.HaXml.Haskell2Xml

data Solvability = Solvable 
		 | Unsolvable 
		   deriving ( Eq , Ord , Show , Read , Typeable )

{-! for Solvability derive: Reader, ToDoc, Haskell2Xml !-}

data Input = Input { matrix      :: [[Integer]]
		   , solvability :: Solvability
		   }
	     deriving ( Typeable )

{-! for Input derive: Reader, ToDoc, Haskell2Xml !-}

ex :: Input
ex = Input { matrix      = [[1,1,0],[1,1,0],[0,0,1]]
	   , solvability = Solvable
	   }
