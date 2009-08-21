{-# LANGUAGE TemplateHaskell #-}

module Pump.Conf where

import qualified Language.Syntax

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Conf z = Conf { lang :: Language.Syntax.Type
		 , samp :: [ String ]
		 , ja_bound :: Int
		 }
     deriving Typeable

$(derives [makeReader, makeToDoc] [''Conf])

-- local variables:
-- mode: haskell
-- end;
