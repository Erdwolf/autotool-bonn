{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Robots.Data where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Data.Typeable

data Robots = Robots deriving ( Typeable )
data Robots_Inverse = Robots_Inverse deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Robots])
$(derives [makeReader, makeToDoc] [''Robots_Inverse])

type Position = ( Integer, Integer )

data Robot = Robot { name :: String
		   , position :: Position
		   , ziel :: Maybe Position
		   }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Robot])

instance Hash Robot where
    hash r = hash ( name r, position r, ziel r )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

$(derives [makeReader, makeToDoc] [''Richtung])

richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

type Zug = ( String, Richtung )

instance Size Zug where size _ = 1

-- local variables:
-- mode: haskell
-- end:
