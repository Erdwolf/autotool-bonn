{-# LANGUAGE TemplateHaskell #-}
module Pump.Quiz where

import Language.Syntax

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Conf z = Conf { lang :: Type
                 , ja_bound :: Int
                 } deriving ( Typeable )

example :: Conf z
example = Conf { lang = Lukas, ja_bound = 10 }

$(derives [makeReader, makeToDoc] [''Conf])
-- {-! for Conf derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end;
