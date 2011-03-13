-- |  (c) F. Pilz, M. Siegburg, J. Waldmann, 2010, License: GPL

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Baum.Heap.Class where

import Baum.Heap.Op
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Hash

import Tree
import Data.Typeable


class Heap baum where

    empty :: baum a
    isEmpty :: baum a -> Bool 

    insert :: Ord a => baum a -> a -> baum a
    -- | precondition: tree is not empty
    deleteMin :: Ord a => baum a -> baum a
    -- | precondition: tree contains this position,
    -- new element is smaller or equal to element at position in tree
    decreaseTo :: Ord a => baum a -> Position -> a -> baum a

    -- true if structure of both trees is equal, false otherwise
    equal :: Eq a => baum a -> baum a -> Bool
    -- neu (nicht in Baum.Such.Class):
    toList :: baum a -> [(Position,a)]

    -- wird vom Aufgaben Generator genutzt und muss implementiert werden
    contents :: baum a -> [a]
    contents b = map snd $ toList b


class ( Show t, Typeable t, Read t
      , Heap baum, OpC a
      , ToTree ( baum a )
      , ToDoc (baum a), Show ( baum a )
      , Reader ( baum a ) , Read ( baum a )
      , ToDot ( baum a), Eq (baum a), Hash (baum a)
      , Typeable1 baum
      , Typeable a
      ) 
    => Tag t baum a | t -> baum, t -> a where
   tag :: t



