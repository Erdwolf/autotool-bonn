{-# OPTIONS -fglasgow-exts #-}

module Robots.Data where

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
		   , ziel :: Maybe Position
		   }
     deriving ( Eq, Ord, Typeable )

{-! for Robot derive : Reader, ToDoc, Haskell2Xml !-}

instance Hash Robot where
    hash r = hash ( name r, position r, ziel r )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

{-! for Richtung derive : Reader, ToDoc, Haskell2Xml !-}

richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

type Zug = ( String, Richtung )

instance Size Zug where size _ = 1

-- local variables:
-- mode: haskell
-- end:
