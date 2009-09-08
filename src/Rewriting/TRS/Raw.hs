{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances -fallow-overlapping-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module Rewriting.TRS.Raw where

import Autolib.Symbol
import Autolib.TES.Term

import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

-- | this is the raw type,
-- needs to be processed because what should be variables
-- will be parsed as nullary symbols by the derived parser
data ( Symbol c, Symbol v ) => TRS v c = 
     TRS { variablen :: [ v ]
         , regeln :: [ Rule ( Term v c ) ]
         }
    deriving ( Eq, Ord, Typeable )



$(derives [makeReader, makeToDoc] [''TRS])
-- {-! for TRS derive: Reader, ToDoc !-}


-- instance Haskell2Xml ( TRS v c  ) --- dummy



-- local variables:
-- mode: haskell
-- end;
