-- -*- mode: haskell -*-

module NPDA.Property where

import NPDA.Type

import Condition

import Autolib.Reporter
import Autolib.Reporter.Type


import Autolib.ToDoc
import Autolib.Reader
import Text.XML.HaXml.Haskell2Xml

import Data.Typeable

data Property = Det
	      | Sane
              | Accept_by Acceptance_Mode
    deriving ( Eq, Ord, Typeable )
{-! for Property derive: Reader, ToDoc, Haskell2Xml !-}

data Acceptance_Mode = Empty_Stack
          | Final_States
    deriving ( Eq, Ord, Typeable )
{-! for Acceptance_Mode derive: Reader, ToDoc, Haskell2Xml !-}

