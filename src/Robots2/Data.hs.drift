{-# OPTIONS -fglasgow-exts #-}

module Robots2.Data where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

data Robots = Robots deriving ( Typeable )
data Robots_Inverse = Robots_Inverse deriving ( Typeable )

{-! for Robots derive : Reader, ToDoc, Haskell2Xml !-}
{-! for Robots_Inverse derive : Reader, ToDoc, Haskell2Xml !-}


type Position = ( Integer, Integer )

data Robot = Robot { name :: String
		   , position :: Position
		   }
     deriving ( Eq, Ord, Typeable )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

{-! for Richtung derive : Reader, ToDoc, Haskell2Xml !-}

richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

type Zug = ( Position, Richtung )

instance Size Zug where size _ = 1

-- local variables:
-- mode: haskell
-- end:
