{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.List.Type 

( ListTree (..)
)


where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable


data ListTree a = Nil
                | Cons a ( ListTree a )
     deriving ( Typeable, Eq )

instance Size ( ListTree a ) where
    size x = 23 

$(derives [makeReader, makeToDoc] [''ListTree])





