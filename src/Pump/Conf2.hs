{-# LANGUAGE TemplateHaskell #-}
module Pump.Conf2 where

import qualified Language.Syntax
import qualified Language.Sampler

import Autolib.ToDoc
import Autolib.Hash
import Autolib.Reader

import Data.Typeable

data Conf z = Conf { sampler :: Language.Sampler.Sampler
		 , ja_bound :: Int
		 }
     deriving ( Typeable )

example :: Conf z
example = Conf { sampler = Language.Sampler.example
	       , ja_bound = 10
	       }

$(derives [makeReader, makeToDoc] [''Conf])
-- {-! for Conf derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
