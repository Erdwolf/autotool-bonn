-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module RedBlackTree.Param where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Autolib.Set

data Param = Param { anzahl :: Int, stellen :: Int }
  deriving ( Typeable )

example :: Param
example = Param { anzahl = 10, stellen = 2 }

$(derives [makeReader, makeToDoc] [''Param])
