{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Lambda.Backward_Join.Instance where

import Lambda.Type
import Lambda.Basic

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Type = Make
          { left_goal :: Lambda
          , right_goal :: Lambda
          }
     deriving ( Typeable, Eq, Ord )

$(derives [makeReader, makeToDoc] [''Type])
-- {-! for Type derive: ToDoc, Reader !-}


-- | problem taken from Barendregt's book
example :: Type
example = Make
        { left_goal  = foldl1 Apply [ k, i, k ] 
        , right_goal = foldl1 Apply [ k, i, s ]
        }

-- local variables:
-- mode: haskell
-- end;