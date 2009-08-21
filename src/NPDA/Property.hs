-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module NPDA.Property where

import NPDA.Type

import Condition

import Autolib.Reporter
import Autolib.Reporter.Type

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Property = Det
	      | Sane
              | Accept_by Acceptance_Mode
    deriving ( Eq, Ord, Typeable )

data Acceptance_Mode = Empty_Stack
          | Final_States
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Property])

$(derives [makeReader, makeToDoc] [''Acceptance_Mode])

