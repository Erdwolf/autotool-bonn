{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Graph.Color where

import Data.Typeable
import Autolib.ToDoc
import Data.Autolib.Transport
import Autolib.Reader
import Autolib.Hash

data Color = A | B | C | D | E | F | G | H | I | J | K | L | M 
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
     deriving ( Eq, Ord, Typeable, Enum, Bounded )

instance Hash Color where hash = hash . fromEnum

$(derives [makeReader, makeToDoc, makeToTransport] [''Color])



