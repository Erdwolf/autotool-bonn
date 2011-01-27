{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.Leftist.Type 

where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data LeftistTree a = Leaf {}
                | Branch { left :: LeftistTree a, key :: a, right :: LeftistTree a }
     deriving ( Typeable, Eq, Show )

$(derives [makeReader, makeToDoc] [''LeftistTree])


instance Functor LeftistTree where
     fmap f t = error "missing instance Functor LeftistTree"


