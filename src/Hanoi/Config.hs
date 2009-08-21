{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Hanoi.Config where

import qualified Hanoi.Restriction as R

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import Gateway.Help

data Config = Config { scheiben :: Int
		 , turme :: Int
		 , restriction :: R.Restriction
		 }
     deriving ( Typeable )

instance Source Config where source _ = drift __FILE__

$(derives [makeReader, makeToDoc] [''Config])

example :: Config
example = Config { scheiben = 6 , turme = 3, restriction = R.Neighbours }

-- local variables:
-- mode: haskell
-- end:

