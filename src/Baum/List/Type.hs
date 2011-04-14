{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

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

instance Functor ListTree where
    fmap f t = case t of
        Nil -> Nil
        Cons x xs -> Cons (f x) $ fmap f xs

$(derives [makeReader, makeToDoc] [''ListTree])





