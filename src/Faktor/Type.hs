{-# language DeriveDataTypeable #-}

module Faktor.Type where

import Data.Typeable
import Autolib.Size

data Faktor = Faktor deriving ( Show , Read, Typeable )

instance Size Integer where size = fromIntegral
