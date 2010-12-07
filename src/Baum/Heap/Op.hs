{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, DeriveDataTypeable #-}

module Baum.Heap.Op where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import Autolib.Reporter
import System.Random

-- | path from root to node.
-- root has position [],
-- left child of root has position [1], etc.
type Position = [ Int ]

class    ( Eq a, Ord a, Show a, ToDoc a, Reader a, Random a ) 
    => OpC a
instance ( Eq a, Ord a, Show a, ToDoc a, Reader a, Random a ) 
    => OpC a

data OpC a => Op a = Insert a | DeleteMin | DecreaseTo Position a | Any
     deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Op])


conforms :: OpC a => Op a -> Op a -> Reporter ()
conforms _ Any = reject $ 
     text "Sie sollen Any durch eine Operation ersetzen."
conforms Any _ = return ()
conforms x y = when ( x /= y ) $ reject $
     text "Die Operation" <+> toDoc x <+> text "soll nicht geändert werden." 

