{-# LANGUAGE TemplateHaskell #-}

module Lambda.Apply where

import Lambda.Type
import Lambda.Basic
import Lambda.Step

import qualified Rewriting.Apply as A

import Rewriting.Derive.Instance

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader


import Control.Monad
import Data.Typeable

data For_Lambda = For_Lambda
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc ] [''For_Lambda])

data Lambda_Calculus = Lambda_Calculus
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc ] [''Lambda_Calculus])

instance  A.Apply For_Lambda Lambda_Calculus 
                       Lambda
                       Int where
    example tag = 
      let sii = Apply ( Apply s i ) i
      in  Instance
        { system = Lambda_Calculus
        , from = Apply sii sii
        , to   = Apply ( Apply i sii) sii
        }
    apply tag system object action = do
        single_derivation object action
    actions tag system object = 
        [ 0 .. pred $ length $ redex_positions object ]

-- local variables:
-- mode: haskell
-- end:
