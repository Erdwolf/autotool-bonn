{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances -fallow-overlapping-instances #-}

module Rewriting.SRS.Raw where

import Autolib.Symbol

import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml


data ( Symbol c ) => SRS c = 
     SRS { regeln :: [ Rule [c] ]
         }
    deriving ( Eq, Ord, Typeable )

example :: SRS Identifier
example = read "SRS { regeln = [ a b -> b a ] }"

{-! for SRS derive: Reader, ToDoc !-}


instance Haskell2Xml ( SRS c  ) --- dummy



-- local variables:
-- mode: haskell
-- end:
