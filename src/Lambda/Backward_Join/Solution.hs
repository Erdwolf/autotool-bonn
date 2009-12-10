{-# LANGUAGE TemplateHaskell #-}

module Lambda.Backward_Join.Solution where

import Lambda.Type

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Type = Make
          { start :: Lambda
          , left_steps :: [ Int ]
          , right_steps :: [ Int ]
          }
     deriving ( Typeable, Eq, Ord )

$(derives [makeReader, makeToDoc] [''Type])

example :: Type
example = Make
        { start = read "(x -> x x)( x -> x x)"
        , left_steps = [0]
        , right_steps = [1]
        }

-- local variables:
-- mode: haskell
-- end;