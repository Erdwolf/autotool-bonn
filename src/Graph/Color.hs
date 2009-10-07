-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Graph.Color where

import Data.Typeable
import Autolib.ToDoc
import Data.Autolib.Transport
import Autolib.Reader
import Autolib.Hash

data Color = A | B | C | D | E | F | G | H
     deriving ( Eq, Ord, Typeable, Enum, Bounded )

instance Hash Color where hash = hash . fromEnum

$(derives [makeReader, makeToDoc, makeToTransport] [''Color])



