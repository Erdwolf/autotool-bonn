{-# OPTIONS -fallow-incoherent-instances -fglasgow-exts #-}

module Baum.Such.Class where

import Baum.Such.Op
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash

import Tree
import Data.Typeable

class Such baum where

    empty :: baum a
    isEmpty :: baum a -> Bool 

    contains :: Ord a => baum a -> a -> Bool
    insert :: Ord a => baum a -> a -> baum a
    delete :: Ord a => baum a -> a -> baum a

    equal :: Eq a => baum a -> baum a -> Bool
    contents :: Ord a => baum a -> [a]

class ( Show t, Typeable t, Read t
      , Such baum, OpC a
      , ToTree ( baum a )
      , ToDoc (baum a), Show ( baum a )
      , Reader ( baum a ) , Read ( baum a )
      , ToDot ( baum a), Eq (baum a), Hash (baum a)
      , Typeable (baum a)
      ) 
    => Tag t baum a | t -> baum, t -> a where
   tag :: t



