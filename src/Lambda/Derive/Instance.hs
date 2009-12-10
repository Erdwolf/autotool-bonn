{-# LANGUAGE TemplateHaskell #-}

module Lambda.Derive.Instance where

import Lambda.Type

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Type = Make
          { from :: Lambda
          , to :: Lambda
          -- | wenn @Just x@, dann genau @x@ Schritte, sonst egal
          , steps :: Maybe Int 
          }
     deriving ( Typeable, Eq, Ord )

$(derives [makeReader, makeToDoc] [''Type])

example :: Type
example = Make
        { from = read "(x -> x x)( x -> x x)"
        , to   = read "(x -> x x)( x -> x x)"
        , steps = Just 2
        }

initial :: Lambda -> Type
initial t = Make 
    { from = t
    , to = t
    , steps = Just 0 
    } 

-- local variables:
-- mode: haskell
-- end