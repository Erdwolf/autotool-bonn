-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Pump.CF.Type where

--   $Id$

import Autolib.Size
import Autolib.Hash
import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Zerlegung = Zerlegung
	       { u :: String, v :: String
	       , x :: String, y :: String, z :: String }
     deriving (Eq, Ord, Typeable)

$(derives [makeReader, makeToDoc] [''Zerlegung])

instance Hash Zerlegung where
    hash e = hash ( hash ( u e, v e ), x e , hash ( y e, z e ) )




