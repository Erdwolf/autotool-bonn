-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Baum.Order where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Order = Pre | In | Post | Level
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Order])

