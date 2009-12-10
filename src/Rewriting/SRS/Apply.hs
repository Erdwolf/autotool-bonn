{-# LANGUAGE TemplateHaskell #-}

module Rewriting.SRS.Apply where

import qualified Rewriting.Apply as A

import Rewriting.Derive.Instance
import Rewriting.SRS.Raw
import Rewriting.SRS.Step
import Rewriting.SRS.Steps

import Autolib.TES.Identifier
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader


import Control.Monad
import Data.Typeable

data For_SRS = For_SRS
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc ] [''For_SRS])

instance  A.Apply For_SRS ( SRS Identifier ) 
                       [ Identifier ]
                       ( Step Identifier ) where
    example tag = Instance
        { system = Rewriting.SRS.Raw.example
        , from = read "[a,a,b,b]"
        , to   = read "[b,b,a,a]"
        }
    apply tag system object action = do
        exec system object action
    actions tag system object = 
        steps system object

-- local variables:
-- mode: haskell
-- end:
