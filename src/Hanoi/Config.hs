{-# OPTIONS -fglasgow-exts -cpp #-}

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

{-! for Config derive: Reader, ToDoc !-}

example :: Config
example = Config { scheiben = 6 , turme = 3, restriction = R.Neighbours }

-- local variables:
-- mode: haskell
-- end:

