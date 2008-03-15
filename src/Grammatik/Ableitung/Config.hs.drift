{-# language DeriveDataTypeable #-}

module Grammatik.Ableitung.Config where

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data Config = Config
	   { max_length :: Int -- wortlänge
	   , max_depth :: Int -- anzahl schichten
	   , max_width :: Int -- breite der schicht
	   }
    deriving ( Typeable )

{-! for Config derive: Reader, ToDoc !-}

-- | als default benutzbar
example :: Config 
example = Config
       { max_length = 10
       , max_depth = 10
       , max_width = 1000
       }

-- local variables:
-- mode: haskell
-- end:
